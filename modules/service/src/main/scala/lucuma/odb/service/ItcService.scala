// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Order
import cats.Parallel
import cats.data.EitherT
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.data.NonEmptyMap
import cats.effect.Async
import cats.effect.Concurrent
import cats.effect.Resource
import cats.effect.syntax.spawn.*
import cats.syntax.applicative.*
import cats.syntax.applicativeError.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.functorFilter.*
import cats.syntax.option.*
import cats.syntax.order.*
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import clue.ResponseException
import fs2.Stream
import io.circe.syntax.*
import lucuma.core.data.Zipper
import lucuma.core.enums.Band
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.util.TimeSpan
import lucuma.itc.AsterismIntegrationTimes
import lucuma.itc.IntegrationTime
import lucuma.itc.ItcGhostDetector
import lucuma.itc.TargetIntegrationTime
import lucuma.itc.client.ClientCalculationResult
import lucuma.itc.client.ImagingInput
import lucuma.itc.client.InstrumentMode
import lucuma.itc.client.ItcClient
import lucuma.itc.client.SpectroscopyInput
import lucuma.itc.client.SpectroscopyParameters
import lucuma.odb.data.Itc
import lucuma.odb.data.Md5Hash
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.data.GeneratorParams
import lucuma.odb.sequence.data.ItcInput
import lucuma.odb.sequence.syntax.hash.*
import lucuma.odb.service.NoTransaction
import lucuma.odb.service.Services.SuperUserAccess
import lucuma.odb.service.Services.Syntax.*
import lucuma.odb.util.Codecs.*
import lucuma.refined.*
import org.typelevel.log4cats.Logger
import skunk.*
import skunk.circe.codec.json.*
import skunk.codec.text.text
import skunk.implicits.*

import scala.concurrent.duration.*

sealed trait ItcService[F[_]] {

  /**
   * Obtains the ITC results for a single target, first checking the cache and
   * then performing a query to the remote ITC service if necessary.
   */
  def lookup(
    programId:     Program.Id,
    observationId: Observation.Id
  )(using NoTransaction[F]): F[Either[OdbError, Itc]]

  /**
   * Using the provided generator parameters, calls the remote ITC service to
   * obtain the AsterismResult, if possible, then caches it for future reference.
   */
  def callRemote(
    programId:     Program.Id,
    observationId: Observation.Id,
    params:        GeneratorParams
  )(using NoTransaction[F]): F[Either[OdbError, Itc]]

  /**
   * Selects the cached ITC result for a single observation, if available and
   * still valid.  
   * Returns Some(Right) for a cached success, Some(Left) for a
   * cached deterministic failure, and None if not cached.
   * Does not perform a remote ITC service call.
   */
  def selectOne(
    programId:    Program.Id,
    observatinId: Observation.Id,
    params:       GeneratorParams
  )(using Transaction[F]): F[Option[Either[OdbError, Itc]]]

  /**
   * Selects the cached ITC results for a program, for those observations where
   * it is available and still valid.  Does not perform a remote ITC service
   * call if not available.
   */
  def selectAll(
    programId: Program.Id,
    params:    Map[Observation.Id, GeneratorParams]
  )(using Transaction[F]): F[Map[Observation.Id, Itc]]

  def selectAll(
    params: Map[Observation.Id, GeneratorParams]
  )(using Transaction[F], SuperUserAccess): F[Map[Observation.Id, Itc]]

}

object ItcService {

  object Error:
    def invalidObservation(
      oid:   Observation.Id,
      error: GeneratorParamsService.Error
    ): OdbError =
      val msg = error match
        case GeneratorParamsService.Error.MissingData(p) =>
          s"ITC cannot be queried until the following parameters are defined: ${p.params.map(_.name).intercalate(", ")}"
        case _                                           =>
          error.format
      OdbError.InvalidObservation(oid, msg.some)

    def itcError(
      problems: NonEmptyList[(Option[Target.Id], String)]
    ): OdbError =
      val ps = problems.map:
        case (None, msg)      => s"Asterism: $msg"
        case (Some(tid), msg) => s"Target '$tid': $msg"
      OdbError.ItcError(s"ITC returned errors: ${ps.intercalate(", ")}".some)

    def targetMismatch: OdbError =
      OdbError.ItcError("ITC provided conflicting results".some)

  end Error

  opaque type Result = Either[OdbError, Itc]

  object Result:
    def apply(e: Either[OdbError, Itc]): Result =
      e

  extension (r: Result)
    def toEither: Either[OdbError, Itc] =
      r

  def pollVersionsForever[F[_]: Async: Logger](
    client:     Resource[F, ItcClient[F]],
    session:    Resource[F, Session[F]],
    pollPeriod: FiniteDuration
  ): F[Unit] = {
    val pollOnce: F[Unit] =
      for {
        v <- client.use(_.versions)
        _ <- session.use { s => s.transaction.use(_ => s.execute(Statements.UpdateItcVersion)(v.serverVersion.some, v.dataVersion)) }
      } yield ()

    val pollOnceLogError: F[Unit] =
      pollOnce.handleErrorWith { t =>
        Logger[F].info(t)(s"Error while polling ITC versions: ${t.getMessage}")
      }

    Stream
      .fixedDelay(pollPeriod)
      .zip(Stream.repeatEval(pollOnceLogError))
      .compile
      .drain
      .start
      .void

  }

  def instantiate[F[_]: Concurrent as F: Parallel: Logger as L: Services](client: ItcClient[F]): ItcService[F] =
    new ItcService[F] {

      override def lookup(
        pid: Program.Id,
        oid: Observation.Id
      )(using NoTransaction[F]): F[Either[OdbError, Itc]] =
        (for {
          pr     <- EitherT(attemptLookup(pid, oid))
          (params, oa) = pr
          result <- oa.fold(EitherT(callRemote(pid, oid, params)))(EitherT.fromEither(_))
        } yield result).value

      override def callRemote(
        pid:    Program.Id,
        oid:    Observation.Id,
        params: GeneratorParams
      )(using NoTransaction[F]): F[Either[OdbError, Itc]] =
        params.itcInput
          .leftMap(m => Error.invalidObservation(oid, GeneratorParamsService.Error.MissingData(m)))
          .fold(
            err => Left(err).pure[F],
            p =>
              callRemoteItc(oid, p).flatTap:
                case Right(r)                       => services.transactionally(insertOrUpdate(pid, oid, p, r))
                case Left(e @ OdbError.ItcError(_)) => services.transactionally(insertOrUpdateFailure(pid, oid, p, e))
                case Left(_)                        => F.unit
          )

      // Selects the parameters then checks the cache
      // Returns None if not cached (call remote), Some(Right) for cached success,
      // Some(Left) for cached deterministic failure.
      private def attemptLookup(
        pid: Program.Id,
        oid: Observation.Id
      )(using NoTransaction[F]): F[Either[OdbError, (GeneratorParams, Option[Either[OdbError, Itc]])]] =
        services.transactionally {
          (for {
            p <- EitherT(generatorParamsService.selectOne(pid, oid).map(_.leftMap(Error.invalidObservation(oid, _))))
            r <- EitherT.liftF(selectOneCached(pid, oid, p))
          } yield (p, r)).value
        }

      private def selectOneCached(
        pid:    Program.Id,
        oid:    Observation.Id,
        params: GeneratorParams
      ): F[Option[Either[OdbError, Itc]]] =
        params.itcInput.toOption.flatTraverse: ps =>
          val inputHash = Md5Hash.unsafeFromByteArray(ps.md5)
          session
            .option(Statements.SelectOneCachedResult)(pid, oid)
            .map: rowOpt =>
              for
                (h, itcOpt, errOpt) <- rowOpt
                if h === inputHash
                result              <- itcOpt.map(_.asRight).orElse(errOpt.map(msg => OdbError.ItcError(msg.some).asLeft))
              yield result

      override def selectOne(
        pid:    Program.Id,
        oid:    Observation.Id,
        params: GeneratorParams
      )(using Transaction[F]): F[Option[Either[OdbError, Itc]]] =
        selectOneCached(pid, oid, params)

      override def selectAll(
        pid:    Program.Id,
        params: Map[Observation.Id, GeneratorParams]
      )(using Transaction[F]): F[Map[Observation.Id, Itc]] =
        session
          .execute(Statements.SelectAllItcResults)(pid)
          .map: (rows: List[(Observation.Id, Md5Hash, Itc)]) =>
            rows.map: (oid, hash, results) =>
              params.get(oid).flatMap(_.itcInput.toOption).flatMap: ps =>
                val inputHash = Md5Hash.unsafeFromByteArray(ps.md5)
                Option.when(hash === inputHash)(oid -> results)
            .flattenOption
            .toMap

      override def selectAll(
        params: Map[Observation.Id, GeneratorParams]
      )(using Transaction[F], SuperUserAccess): F[Map[Observation.Id, Itc]] =
        NonEmptyList.fromList(params.keys.toList).fold(Map.empty.pure[F]): nel =>
          val enc = observation_id.nel(nel)
          session
            .execute(Statements.selectAllItcResults(enc))(nel)
            .map: rows =>
              rows
                .flatMap: (oid, hash, results) =>
                  for
                    params <- params.get(oid)
                    input  <- params.itcInput.toOption
                    inhash  = Md5Hash.unsafeFromByteArray(input.md5)
                    pair   <- Option.when(hash === inhash)(oid -> results)
                  yield pair
                .toMap


      // According to the spec we default if the target is too bright
      // https://app.shortcut.com/lucuma/story/1999/determine-exposure-time-for-acquisition-images
      private def safeAcquisitionCall(
        oid:     Observation.Id,
        input:   ImagingInput,
        targets: NonEmptyList[ItcInput.TargetDefinition]
      ): EitherT[F, OdbError, Zipper[Itc.Result]] =
        def go(min: TimeSpan, max: TimeSpan): EitherT[F, OdbError, Zipper[Itc.Result]] =
          EitherT:
            client
              .imaging(input, useCache = false)
              .map: ccr =>
                val modifiedTargetTimes =
                  ccr.targetTimes.modifyValue:
                    _.map:
                      _.modifyValue:
                        case Left(lucuma.itc.Error.SourceTooBright(_))    =>
                          TargetIntegrationTime(
                            Zipper.one(IntegrationTime(min, 1.refined)),
                            Band.R.asLeft, // Band is meaningless here, but we need to provide one
                            None, // Imaging doesn't return signal-to-noise at
                            Nil // No ccd data for this case
                          ).asRight
                        case Right(r) if r.times.focus.exposureTime > max =>
                          r.copy(times = r.times.map(_.copy(exposureTime = max))).asRight
                        case Right(r) if r.times.focus.exposureTime < min =>
                          r.copy(times = r.times.map(_.copy(exposureTime = min))).asRight
                        case other => other

                val modifiedCcr = ClientCalculationResult(ccr.versions, modifiedTargetTimes)
                toTargetResults(targets, NonEmptyList.one(modifiedCcr)).map(_.head)
              .handleErrorWith:
                case e: ResponseException[?] =>
                  OdbError.ItcError(e.getMessage.some).asLeft.pure
                case t =>
                  OdbError.RemoteServiceCallError(s"Error calling ITC service: ${t.getMessage}".some).asLeft.pure

        input.mode match
          case InstrumentMode.GmosNorthImaging(_, _, _, _) |
               InstrumentMode.GmosSouthImaging(_, _, _, _) =>
            go(lucuma.odb.sequence.gmos.MinAcquisitionExposureTime,
               lucuma.odb.sequence.gmos.MaxAcquisitionExposureTime)
          case InstrumentMode.Flamingos2Imaging(_, _, _, _) =>
            go(lucuma.odb.sequence.flamingos2.MinAcquisitionExposureTime,
               lucuma.odb.sequence.flamingos2.MaxAcquisitionExposureTime)
          case InstrumentMode.GnirsImaging(_, _, _, _, _, _, _) =>
            go(lucuma.odb.sequence.gnirs.MinAcquisitionExposureTime,
               lucuma.odb.sequence.gnirs.MaxAcquisitionExposureTime)
          case m                                                      =>
            EitherT.leftT:
              OdbError.InvalidObservation(oid, s"Acquisition is not supported for ${m.displayName}".some)

      private def toTargetResults(
        targets: NonEmptyList[ItcInput.TargetDefinition],
        results: NonEmptyList[ClientCalculationResult],
        signalToNoiseTargetId: Option[Target.Id] = none
      ): Either[OdbError, NonEmptyList[Zipper[Itc.Result]]] =

        def convertErrors(
          itcErrors: NonEmptyChain[(lucuma.itc.Error, Int)]
        ): OdbError =
          Error.itcError:
            itcErrors.map { case (e, i) =>
              targets.get(i).map(_.targetId) -> e.message
            }.toNonEmptyList

        results.traverse: r =>
          r.targetTimes
           .partitionErrors
           .leftMap(convertErrors)
           .map: a =>
             val z = a.value.zipWithIndex.map { case (intTime, index) =>
               val t = targets.get(index).get
               Itc.Result(t.targetId, intTime.times.focus, intTime.signalToNoiseAt)
             }
             // Pin the "selected" result to the user's signal-to-noise target,
             // if one is set and present; otherwise keep the automatic
             // (brightest) focus.
             signalToNoiseTargetId
               .flatMap(id => z.findFocus(_.targetId === id))
               .getOrElse(z)

      sealed trait Imaging[A]:
        def pf: PartialFunction[InstrumentMode, A]
        def wrap(nem: NonEmptyMap[A, Zipper[Itc.Result]]): Itc

      object Imaging:
        case object Flamingos2Imaging extends Imaging[Flamingos2Filter]:
          override def pf: PartialFunction[InstrumentMode, Flamingos2Filter] =
            case InstrumentMode.Flamingos2Imaging(filter = f) => f

          override def wrap(nem: NonEmptyMap[Flamingos2Filter, Zipper[Itc.Result]]): Itc =
            Itc.Flamingos2Imaging(nem)

        case object GmosNorthImaging extends Imaging[GmosNorthFilter]:
          override def pf: PartialFunction[InstrumentMode, GmosNorthFilter] =
            case InstrumentMode.GmosNorthImaging(filter = f) => f

          override def wrap(nem: NonEmptyMap[GmosNorthFilter, Zipper[Itc.Result]]): Itc =
            Itc.GmosNorthImaging(nem)

        case object GmosSouthImaging extends Imaging[GmosSouthFilter]:
          override def pf: PartialFunction[InstrumentMode, GmosSouthFilter] =
               case InstrumentMode.GmosSouthImaging(filter = f) => f

          override def wrap(nem: NonEmptyMap[GmosSouthFilter, Zipper[Itc.Result]]): Itc =
            Itc.GmosSouthImaging(nem)

      private def callRemoteImagingItc[A: Order](
        oid:   Observation.Id,
        input: ItcInput.Imaging,
        im:    Imaging[A]
      ): EitherT[F, OdbError, Itc] =

        def extractFilters(
          remaining: List[InstrumentMode],
          filters:   List[A]
        ): Either[OdbError, NonEmptyList[A]] =
          remaining match
            case Nil    =>
              // remaining originally comes from a NonEmptyList
              NonEmptyList.fromListUnsafe(filters.reverse).asRight
            case h :: t =>
              if im.pf.isDefinedAt(h) then extractFilters(t, im.pf(h) :: filters)
              else OdbError.InvalidObservation(oid, s"Mixed instrument mode observations are not supported.".some).asLeft

        val clientCalculationResults: F[Either[OdbError, NonEmptyList[ClientCalculationResult]]] =
          input
            .scienceInput
            .parTraverse: in =>
              client.imaging(in, useCache = false)
            .map(_.asRight)
            .handleErrorWith:
              case e: ResponseException[?] =>
                OdbError.ItcError(e.getMessage.some).asLeft.pure
              case t =>
                OdbError.RemoteServiceCallError(s"Error calling ITC service: ${t.getMessage}".some).asLeft.pure

        for
          fs <- EitherT.fromEither(extractFilters(input.science.map(_.mode).toList, Nil))
          cs <- EitherT(clientCalculationResults)
          ts <- EitherT.fromEither(toTargetResults(input.targets, cs, input.signalToNoiseTargetId))
        yield im.wrap(fs.zip(ts).toNem)


      private def callRemoteItc(
        oid:   Observation.Id,
        input: ItcInput
      )(using NoTransaction[F]): F[Either[OdbError, Itc]] =

        def callSpectroscopy(si: SpectroscopyInput): EitherT[F, OdbError, ClientCalculationResult] =
          EitherT:
            client
              .spectroscopy(si, useCache = false)
              .map(_.asRight)
              .handleErrorWith:
                case e: ResponseException[?] =>
                  OdbError.ItcError(e.getMessage.some).asLeft.pure
                case t =>
                  OdbError.RemoteServiceCallError(s"Error calling ITC service: ${t.getMessage}".some).asLeft.pure

        // A placeholder result for GHOST, at least for now.
        //
        // * The ITC accepts the red and blue exposure time mode data, but
        //   only returns a single result set?  It isn't clear how to map this
        //   back onto the red and blue channels.
        //
        // * The ITC only handles Time And Count mode and we don't use the S/N
        //   value so we'll just short-circuit the call and make a fake result
        //   that has the time and count we need per channel.
        def ghost(
          ghost:   InstrumentMode.GhostSpectroscopy,
          targets: NonEmptyList[ItcInput.TargetDefinition]
        ): EitherT[F, OdbError, Itc] =
          def resultSet(d: ItcGhostDetector): Zipper[Itc.Result] =
            Zipper.fromNel:
              targets.map: t =>
                val it = IntegrationTime(d.timeAndCount.time, d.timeAndCount.count)
                Itc.Result(t.targetId, it, none)

          EitherT.pure:
            Itc.GhostIfu(
              resultSet(ghost.redDetector),
              resultSet(ghost.blueDetector)
            )

        def imaging(im: ItcInput.Imaging): EitherT[F, OdbError, Itc] =
          im.science.head.mode match
            case InstrumentMode.Flamingos2Imaging(_, _, _, _) =>
              callRemoteImagingItc(oid, im, Imaging.Flamingos2Imaging)

            case InstrumentMode.GmosNorthImaging(_, _, _, _) =>
              callRemoteImagingItc(oid, im, Imaging.GmosNorthImaging)

            case InstrumentMode.GmosSouthImaging(_, _, _, _) =>
              callRemoteImagingItc(oid, im, Imaging.GmosSouthImaging)

            case m                                     =>
              EitherT.leftT:
                OdbError.InvalidObservation(oid, s"Imaging ITC lookup is not supported for ${m.displayName}.".some)

        def spectroscopy(sp: ItcInput.Spectroscopy): EitherT[F, OdbError, Itc] =
          for
            cr  <- callSpectroscopy(sp.scienceInput)
            sci <- EitherT.fromEither(toTargetResults(sp.targets, NonEmptyList.one(cr), sp.signalToNoiseTargetId).map(_.head))
            acq <- safeAcquisitionCall(oid, sp.acquisitionInput, sp.acquisitionTargets)
          yield Itc.Spectroscopy(acq, sci)

        def igrins2Spectroscopy(sp: ItcInput.ScienceOnlySpectroscopy): EitherT[F, OdbError, Itc] =
          for
            cr  <- callSpectroscopy(sp.scienceInput)
            sci <- EitherT.fromEither(toTargetResults(sp.targets, NonEmptyList.one(cr), sp.signalToNoiseTargetId).map(_.head))
          yield Itc.Igrins2Spectroscopy(sci)

        (input match
          case im @ ItcInput.Imaging(_, _, _) =>
            imaging(im)
          case sp @ ItcInput.Spectroscopy(_, _, _, _, _) =>
            spectroscopy(sp)
          case sp @ ItcInput.ScienceOnlySpectroscopy(SpectroscopyParameters(_, gh @ InstrumentMode.GhostSpectroscopy(_, _, _, _)), targets, _) =>
            ghost(gh, targets)
          case sp @ ItcInput.ScienceOnlySpectroscopy(SpectroscopyParameters(_, InstrumentMode.Igrins2Spectroscopy(_, _)), _, _) =>
            igrins2Spectroscopy(sp)
          case _ =>
            EitherT.leftT(OdbError.InvalidObservation(oid, s"Unrecognized ItcInput: $input".some))
        ).value

      private def insertOrUpdate(
        pid:     Program.Id,
        oid:     Observation.Id,
        input:   ItcInput,
        results: Itc
      ): F[Unit] =
        val h = Md5Hash.unsafeFromByteArray(input.md5)
        session.execute(Statements.InsertOrUpdateItcResult)(pid, oid, h, results, h, results)
          .void
          .recoverWith {
            case SqlState.ForeignKeyViolation(ex) =>
              L.info(ex)(s"Failed to insert or update ITC result for program $pid, observation $oid. Probably due to a deleted calibration observation.")
          }

      private def insertOrUpdateFailure(
        pid:   Program.Id,
        oid:   Observation.Id,
        input: ItcInput,
        error: OdbError.ItcError
      ): F[Unit] =
        val h   = Md5Hash.unsafeFromByteArray(input.md5)
        val msg = error.detail.getOrElse("")
        session.execute(Statements.InsertOrUpdateItcFailure)(pid, oid, h, msg, h, msg)
          .void
          .recoverWith:
            case SqlState.ForeignKeyViolation(ex) =>
              L.info(ex)(s"Failed to insert or update ITC failure for program $pid, observation $oid. Probably due to a deleted calibration observation.")
    }

  object Statements {
    import lucuma.odb.json.itc.given
    import lucuma.odb.json.time.transport.given
    import lucuma.odb.json.wavelength.transport.given

    private val itc: Codec[Itc] =
      jsonb.eimap(
        _.as[Itc].leftMap(f => s"Could not decode ITC results: ${f.message}")
      )(_.asJson)

    val UpdateItcVersion: Command[(
      Option[String],
      Option[String]
    )] =
      sql"""
        UPDATE t_itc_version
           SET c_version = ${text.opt},
               c_data    = ${text.opt}
      """.command

    val SelectOneCachedResult: Query[(
      Program.Id,
      Observation.Id,
    ), (Md5Hash, Option[Itc], Option[String])] =
      sql"""
        SELECT
          c_hash,
          c_asterism_results,
          c_error_message
        FROM t_itc_result
        WHERE c_program_id     = $program_id     AND
              c_observation_id = $observation_id
      """.query(md5_hash *: itc.opt *: text.opt)

    val SelectAllItcResults: Query[Program.Id, (Observation.Id, Md5Hash, Itc)] =
      sql"""
        SELECT
          c_observation_id,
          c_hash,
          c_asterism_results
        FROM t_itc_result
        WHERE c_program_id         = $program_id AND
              c_asterism_results IS NOT NULL
      """.query(observation_id *: md5_hash *: itc)

    def selectAllItcResults[A <: NonEmptyList[Observation.Id]](enc: skunk.Encoder[A]): Query[A, (Observation.Id, Md5Hash, Itc)] =
      sql"""
        SELECT
          c_observation_id,
          c_hash,
          c_asterism_results
        FROM t_itc_result
        WHERE c_observation_id    IN ($enc) AND
              c_asterism_results IS NOT NULL
      """.query(observation_id *: md5_hash *: itc)

    val InsertOrUpdateItcResult: Command[(
      Program.Id,
      Observation.Id,
      Md5Hash,
      Itc,
      Md5Hash,
      Itc
    )] =
      sql"""
        INSERT INTO t_itc_result (
          c_program_id,
          c_observation_id,
          c_hash,
          c_asterism_results
        ) VALUES (
          $program_id,
          $observation_id,
          $md5_hash,
          $itc
        )
        ON CONFLICT ON CONSTRAINT t_itc_result_pkey DO UPDATE
          SET c_hash             = $md5_hash,
              c_asterism_results = $itc,
              c_error_message    = NULL
      """.command

    val InsertOrUpdateItcFailure: Command[(
      Program.Id,
      Observation.Id,
      Md5Hash,
      String,
      Md5Hash,
      String
    )] =
      sql"""
        INSERT INTO t_itc_result (
          c_program_id,
          c_observation_id,
          c_hash,
          c_error_message
        ) VALUES (
          $program_id,
          $observation_id,
          $md5_hash,
          $text
        )
        ON CONFLICT ON CONSTRAINT t_itc_result_pkey DO UPDATE
          SET c_hash             = $md5_hash,
              c_asterism_results = NULL,
              c_error_message    = $text
      """.command

  }

}
