// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.EitherT
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
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
import cats.syntax.traverse.*
import fs2.Stream
import io.circe.syntax.*
import lucuma.core.data.Zipper
import lucuma.core.enums.Band
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.util.TimeSpan
import lucuma.itc.AsterismIntegrationTimes
import lucuma.itc.IntegrationTime
import lucuma.itc.TargetIntegrationTime
import lucuma.itc.client.ImagingInput
import lucuma.itc.client.InstrumentMode
import lucuma.itc.client.ItcClient
import lucuma.odb.data.Itc
import lucuma.odb.data.Itc.ModeData
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
   * Selects the cached ITC results for a single observation, if available and
   * still valid.  Does not perform a remote ITC service call if not available.
   */
  def selectOne(
    programId:    Program.Id,
    observatinId: Observation.Id,
    params:       GeneratorParams
  )(using Transaction[F]): F[Option[Itc]]

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

  def instantiate[F[_]: Concurrent: Logger](client: ItcClient[F])(using Services[F]): ItcService[F] =
    new ItcService[F] {

      override def lookup(
        pid: Program.Id,
        oid: Observation.Id
      )(using NoTransaction[F]): F[Either[OdbError, Itc]] =
        (for {
          pr     <- EitherT(attemptLookup(pid, oid))
          (params, oa) = pr
          result <- oa.fold(EitherT(callRemote(pid, oid, params)))(EitherT.pure(_))
        } yield result).value

      override def callRemote(
        pid:    Program.Id,
        oid:    Observation.Id,
        params: GeneratorParams
      )(using NoTransaction[F]): F[Either[OdbError, Itc]] =
        (for {
          p <- EitherT.fromEither(params.itcInput.leftMap(m => Error.invalidObservation(oid, GeneratorParamsService.Error.MissingData(m))))
          r <- EitherT(callRemoteItc(oid, p))
          _ <- EitherT.liftF(services.transactionally(insertOrUpdate(pid, oid, p, r)))
        } yield r).value

      // Selects the parameters then selects the previously stored result set, if any.
      private def attemptLookup(
        pid: Program.Id,
        oid: Observation.Id
      )(using NoTransaction[F]): F[Either[OdbError, (GeneratorParams, Option[Itc])]] =
        services.transactionally {
          (for {
            p <- EitherT(generatorParamsService.selectOne(pid, oid).map(_.leftMap(Error.invalidObservation(oid, _))))
            r <- EitherT.liftF(selectOne(pid, oid, p))
          } yield (p, r)).value
        }

      override def selectOne(
        pid:    Program.Id,
        oid:    Observation.Id,
        params: GeneratorParams
      )(using Transaction[F]): F[Option[Itc]] =
        params.itcInput.toOption.flatTraverse: ps =>
          val inputHash = Md5Hash.unsafeFromByteArray(ps.md5)
          session
            .option(Statements.SelectOneItcResult)(pid, oid)
            .map:
              _.collect { case (h, rs) if h === inputHash => rs }

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

      private def convertErrors(targets: ItcInput)(itcErrors: NonEmptyChain[(lucuma.itc.Error, Int)]): OdbError =
        Error.itcError(itcErrors.map { case (e, i) => (targets.targetVector.get(i).map(_._1), e.message) }.toNonEmptyList)

      // According to the spec we default if the target is too bright
      // https://app.shortcut.com/lucuma/story/1999/determine-exposure-time-for-acquisition-images
      private def safeAcquisitionCall(targets: ItcInput): F[Option[Either[OdbError, AsterismIntegrationTimes]]] =
        def go(input: ImagingInput, min: TimeSpan, max: TimeSpan): F[Either[OdbError, AsterismIntegrationTimes]] =
          client
            .imaging(input, useCache = false)
            .map:
              _.targetTimes.modifyValue:
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
              .partitionErrors
              .leftMap(convertErrors(targets))

        targets.acquisitionInput.traverse: in =>

          in.mode match
            case InstrumentMode.GmosNorthSpectroscopy(_, _, _, _, _, _) |
                 InstrumentMode.GmosSouthSpectroscopy(_, _, _, _, _, _) |
                 InstrumentMode.GmosNorthImaging(_, _)                  |
                 InstrumentMode.GmosSouthImaging(_, _)                    =>
              go(in,
                 lucuma.odb.sequence.gmos.MinAcquisitionExposureTime,
                 lucuma.odb.sequence.gmos.MaxAcquisitionExposureTime)
            case InstrumentMode.Flamingos2Spectroscopy(_, _, _)         |
                 InstrumentMode.Flamingos2Imaging(_)                      =>
              go(in,
                 lucuma.odb.sequence.flamingos2.MinAcquisitionExposureTime,
                 lucuma.odb.sequence.flamingos2.MaxAcquisitionExposureTime)

      private def callRemoteItc(
        oid:     Observation.Id,
        targets: ItcInput
      )(using NoTransaction[F]): F[Either[OdbError, Itc]] =

        // TODO: for now support only a single science result. We have all the
        // inputs we need now though in order to call ITC repeatedly and process
        // all the results.

        def toTargetResults(a: AsterismIntegrationTimes): Zipper[Itc.Result] =
          a.value.zipWithIndex.map { case (targetIntegrationTime, index) =>
            val (targetId, targetInput) = targets.targetVector.getUnsafe(index)
            Itc.Result(targetId, targetIntegrationTime.times.focus, targetIntegrationTime.signalToNoiseAt)
          }


        val res = for
          acq <- safeAcquisitionCall(targets)
          img <- targets.imagingInputs.headOption.traverse(input => client.imaging(input, useCache = false))
          spc <- targets.spectroscopyInputs.headOption.traverse(input => client.spectroscopy(input, useCache = false))
        yield
          val sci = img.orElse(spc).toRight(OdbError.InvalidObservation(oid, s"The observation has neither imaging nor spectroscopy mode.".some))

          val sciZipper: Either[OdbError, Zipper[Itc.Result]] =
            sci.flatMap: sciResult =>
              sciResult
                .targetTimes
                .partitionErrors
                .leftMap(convertErrors(targets))
                .map(toTargetResults)

          val acqZipper: Option[Either[OdbError, Zipper[Itc.Result]]] =
            acq.map(_.map(toTargetResults))

          val res = acqZipper.fold(sciZipper.map(sz => Itc.fromResults(ModeData.Empty, none, sz))): eaz =>
            eaz.flatMap: az =>
              sciZipper.flatMap: sz =>
                Itc.fromResults(ModeData.Empty, az.some, sz).asRight[OdbError]

          res.flatMap(_.toRight(Error.targetMismatch))

        res.handleError: t =>
          OdbError.RemoteServiceCallError(s"Error calling ITC service: ${t.getMessage}".some).asLeft

      @annotation.nowarn("msg=unused implicit parameter")
      private def insertOrUpdate(
        pid:     Program.Id,
        oid:     Observation.Id,
        input:   ItcInput,
        results: Itc
      )(using Transaction[F]): F[Unit] =
        val h = Md5Hash.unsafeFromByteArray(input.md5)
        session.execute(Statements.InsertOrUpdateItcResult)(pid, oid, h, results, h, results)
          .void
          .recoverWith {
            case SqlState.ForeignKeyViolation(ex) =>
              Logger[F].info(ex)(s"Failed to insert or update ITC result for program $pid, observation $oid. Probably due to a deleted calibration observation.")
          }
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

    val SelectOneItcResult: Query[(
      Program.Id,
      Observation.Id,
    ), (Md5Hash, Itc)] =
      sql"""
        SELECT
          c_hash,
          c_asterism_results
        FROM t_itc_result
        WHERE c_program_id     = $program_id     AND
              c_observation_id = $observation_id
      """.query(md5_hash *: itc)

    val SelectAllItcResults: Query[Program.Id, (Observation.Id, Md5Hash, Itc)] =
      sql"""
        SELECT
          c_observation_id,
          c_hash,
          c_asterism_results
        FROM t_itc_result
        WHERE c_program_id = $program_id
      """.query(observation_id *: md5_hash *: itc)

    def selectAllItcResults[A <: NonEmptyList[Observation.Id]](enc: skunk.Encoder[A]): Query[A, (Observation.Id, Md5Hash, Itc)] =
      sql"""
        SELECT
          c_observation_id,
          c_hash,
          c_asterism_results
        FROM t_itc_result
        WHERE c_observation_id IN ($enc)
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
        ) SELECT
          $program_id,
          $observation_id,
          $md5_hash,
          $itc
        ON CONFLICT ON CONSTRAINT t_itc_result_pkey DO UPDATE
          SET c_hash             = $md5_hash,
              c_asterism_results = $itc
      """.command

  }

}
