// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Order
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
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.PosInt
import fs2.Stream
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.Json
import io.circe.refined.*
import io.circe.syntax.*
import lucuma.core.data.Zipper
import lucuma.core.data.ZipperCodec.given
import lucuma.core.enums.Band
import lucuma.core.math.SignalToNoise
import lucuma.core.math.SingleSN
import lucuma.core.math.TotalSN
import lucuma.core.math.Wavelength
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.util.TimeSpan
import lucuma.itc.AsterismIntegrationTimes
import lucuma.itc.IntegrationTime
import lucuma.itc.SignalToNoiseAt
import lucuma.itc.TargetIntegrationTime
import lucuma.itc.client.InstrumentMode
import lucuma.itc.client.ItcClient
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

  import ItcService.AsterismResults

  /**
   * Obtains the ITC results for a single target, first checking the cache and
   * then performing a query to the remote ITC service if necessary.
   */
  def lookup(
    programId:     Program.Id,
    observationId: Observation.Id
  )(using NoTransaction[F]): F[Either[OdbError, AsterismResults]]

  /**
   * Using the provided generator parameters, calls the remote ITC service to
   * obtain the AsterismResult, if possible, then caches it for future reference.
   */
  def callRemote(
    programId:     Program.Id,
    observationId: Observation.Id,
    params:        GeneratorParams
  )(using NoTransaction[F]): F[Either[OdbError, AsterismResults]]

  /**
   * Selects the cached ITC results for a single observation, if available and
   * still valid.  Does not perform a remote ITC service call if not available.
   */
  def selectOne(
    programId:    Program.Id,
    observatinId: Observation.Id,
    params:       GeneratorParams
  )(using Transaction[F]): F[Option[AsterismResults]]

  /**
   * Selects the cached ITC results for a program, for those observations where
   * it is available and still valid.  Does not perform a remote ITC service
   * call if not available.
   */
  def selectAll(
    programId: Program.Id,
    params:    Map[Observation.Id, GeneratorParams]
  )(using Transaction[F]): F[Map[Observation.Id, AsterismResults]]

  def selectAll(
    params: Map[Observation.Id, GeneratorParams]
  )(using Transaction[F], SuperUserAccess): F[Map[Observation.Id, AsterismResults]]

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

  case class TargetResult(targetId: Target.Id, value: IntegrationTime, signalToNoise: Option[SignalToNoiseAt]) {
    def totalTime: Option[TimeSpan] = {
      val total = BigInt(value.exposureTime.toMicroseconds) * value.exposureCount.value
      Option.when(total.isValidLong)(TimeSpan.fromMicroseconds(total.longValue)).flatten
    }
  }

  object TargetResult {
    given Order[TargetResult] =
      Order.by { s => (s.totalTime, s.targetId) }

    import lucuma.odb.json.time.query.given
    import lucuma.odb.json.wavelength.query.given

    given Decoder[SignalToNoiseAt] = c =>
      for {
        w <- c.downField("wavelength").as[Wavelength]
        s <- c.downField("single").as[SignalToNoise]
        t <- c.downField("total").as[SignalToNoise]
      } yield SignalToNoiseAt(w, SingleSN(s), TotalSN(t))

    given Decoder[TargetResult] =
      Decoder.instance { c =>
        for {
          targetId        <- c.downField("targetId").as[Target.Id]
          exposureTime    <- c.downField("exposureTime").as[TimeSpan]
          exposureCount   <- c.downField("exposureCount").as[PosInt]
          signalToNoiseAt <- c.downField("signalToNoiseAt").as[Option[SignalToNoiseAt]]
        } yield TargetResult(targetId, IntegrationTime(exposureTime, exposureCount), signalToNoiseAt)
      }

    given Encoder[TargetResult] =
      Encoder.instance { s =>
        Json.obj(
          "targetId"        -> s.targetId.asJson,
          "exposureTime"    -> s.value.exposureTime.asJson,
          "exposureCount"   -> s.value.exposureCount.value.asJson,
          "signalToNoiseAt" -> s.signalToNoise.asJson
        )
      }
  }

  case class AsterismResults private (
    acquisitionResult: Zipper[TargetResult],
    scienceResult: Zipper[TargetResult]
  )

  object AsterismResults {

    def fromResults(acquisition: Zipper[TargetResult], science: Zipper[TargetResult]): Option[AsterismResults] =
      if (acquisition.toNel.map(_.targetId).sortBy(_.value) === science.toNel.map(_.targetId).sortBy(_.value)) {
        AsterismResults(acquisition, science).some
      } else none

    given Encoder[AsterismResults] =
      Encoder.instance { rs =>
        Json.obj(
          "science" -> rs.scienceResult.widen[TargetResult].asJson,
          "acquisition" -> rs.acquisitionResult.widen[TargetResult].asJson
        )
      }

    given Decoder[AsterismResults] =
      Decoder.instance { c =>
        for {
          science     <- c.downField("science").as[Zipper[TargetResult]]
          acquisition <- c.downField("acquisition").as[Zipper[TargetResult]]
          result      <- fromResults(acquisition, science).toRight(DecodingFailure("Target mismatch", c.history))
        } yield result
      }
  }

  opaque type Result = Either[OdbError, AsterismResults]

  object Result:
    def apply(e: Either[OdbError, AsterismResults]): Result =
      e

  extension (r: Result)
    def toEither: Either[OdbError, AsterismResults] =
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
      )(using NoTransaction[F]): F[Either[OdbError, AsterismResults]] =
        (for {
          pr     <- EitherT(attemptLookup(pid, oid))
          (params, oa) = pr
          result <- oa.fold(EitherT(callRemote(pid, oid, params)))(EitherT.pure(_))
        } yield result).value

      override def callRemote(
        pid:    Program.Id,
        oid:    Observation.Id,
        params: GeneratorParams
      )(using NoTransaction[F]): F[Either[OdbError, AsterismResults]] =
        (for {
          p <- EitherT.fromEither(params.itcInput.leftMap(m => Error.invalidObservation(oid, GeneratorParamsService.Error.MissingData(m))))
          r <- EitherT(callRemoteItc(oid, p))
          _ <- EitherT.liftF(services.transactionally(insertOrUpdate(pid, oid, p, r)))
        } yield r).value

      // Selects the parameters then selects the previously stored result set, if any.
      private def attemptLookup(
        pid: Program.Id,
        oid: Observation.Id
      )(using NoTransaction[F]): F[Either[OdbError, (GeneratorParams, Option[AsterismResults])]] =
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
      )(using Transaction[F]): F[Option[AsterismResults]] =
        params.itcInput.toOption.flatTraverse: ps =>
          val inputHash = Md5Hash.unsafeFromByteArray(ps.md5)
          session
            .option(Statements.SelectOneItcResult)(pid, oid)
            .map:
              _.collect { case (h, rs) if h === inputHash => rs }

      override def selectAll(
        pid:    Program.Id,
        params: Map[Observation.Id, GeneratorParams]
      )(using Transaction[F]): F[Map[Observation.Id, AsterismResults]] =
        session
          .execute(Statements.SelectAllItcResults)(pid)
          .map: (rows: List[(Observation.Id, Md5Hash, AsterismResults)]) =>
            rows.map: (oid, hash, results) =>
              params.get(oid).flatMap(_.itcInput.toOption).flatMap: ps =>
                val inputHash = Md5Hash.unsafeFromByteArray(ps.md5)
                Option.when(hash === inputHash)(oid -> results)
            .flattenOption
            .toMap

      override def selectAll(
        params: Map[Observation.Id, GeneratorParams]
      )(using Transaction[F], SuperUserAccess): F[Map[Observation.Id, AsterismResults]] =
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
      private def safeAcquisitionCall(targets: ItcInput): F[Either[OdbError, AsterismIntegrationTimes]] =
        def go(min: TimeSpan, max: TimeSpan): F[Either[OdbError, AsterismIntegrationTimes]] =
          client
            .imaging(targets.acquisitionInput, useCache = false)
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

        targets.acquisitionInput.mode match
          case InstrumentMode.GmosNorthSpectroscopy(_, _, _, _, _, _) |
               InstrumentMode.GmosSouthSpectroscopy(_, _, _, _, _, _) |
               InstrumentMode.GmosNorthImaging(_, _)                  |
               InstrumentMode.GmosSouthImaging(_, _)                    =>
            go(lucuma.odb.sequence.gmos.MinAcquisitionExposureTime,
               lucuma.odb.sequence.gmos.MaxAcquisitionExposureTime)
          case InstrumentMode.Flamingos2Spectroscopy(_, _, _)         |
               InstrumentMode.Flamingos2Imaging(_)                      =>
            go(lucuma.odb.sequence.flamingos2.MinAcquisitionExposureTime,
               lucuma.odb.sequence.flamingos2.MaxAcquisitionExposureTime)

      private def callRemoteItc(
        oid:     Observation.Id,
        targets: ItcInput
      )(using NoTransaction[F]): F[Either[OdbError, AsterismResults]] =

        // TODO: for now support only a single science result. We have all the
        // inputs we need now though in order to call ITC repeatedly and process
        // all the results.

        val res = for
          acq <- safeAcquisitionCall(targets)
          img <- targets.imagingInputs.headOption.traverse(input => client.imaging(input, useCache = false))
          spc <- targets.spectroscopyInputs.headOption.traverse(input => client.spectroscopy(input, useCache = false))
        yield
          val sci = img.orElse(spc).toRight(OdbError.InvalidObservation(oid, s"The observation has neither imaging nor spectroscopy mode.".some))
          acq.flatMap: acqResult =>
            sci.flatMap: sciResult =>
              sciResult
                .targetTimes
                .partitionErrors
                .leftMap(convertErrors(targets))
                .flatMap: sciResult =>
                  AsterismResults.fromResults(
                    acqResult.value.zipWithIndex.map { case (targetIntegrationTime, index) =>
                      val (targetId, targetInput) = targets.targetVector.getUnsafe(index)
                      TargetResult(targetId, /*(targets.imaging, targetInput),*/ targetIntegrationTime.times.focus, targetIntegrationTime.signalToNoiseAt)
                    },
                    sciResult.value.zipWithIndex.map { case (targetIntegrationTime, index) =>
                      val (targetId, targetInput) = targets.targetVector.getUnsafe(index)
                      TargetResult(targetId, /*(targets.spectroscopy, targetInput),*/ targetIntegrationTime.times.focus, targetIntegrationTime.signalToNoiseAt)
                    }
                  ).toRight(Error.targetMismatch)

        res.handleError: t =>
          OdbError.RemoteServiceCallError(s"Error calling ITC service: ${t.getMessage}".some).asLeft

      @annotation.nowarn("msg=unused implicit parameter")
      private def insertOrUpdate(
        pid:     Program.Id,
        oid:     Observation.Id,
        input:   ItcInput,
        results: AsterismResults
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
    private val asterism_results: Codec[AsterismResults] =
      jsonb.eimap(
        _.as[AsterismResults].leftMap(f => s"Could not decode AsterismResults: ${f.message}")
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
    ), (Md5Hash, AsterismResults)] =
      sql"""
        SELECT
          c_hash,
          c_asterism_results
        FROM t_itc_result
        WHERE c_program_id     = $program_id     AND
              c_observation_id = $observation_id
      """.query(md5_hash *: asterism_results)

    val SelectAllItcResults: Query[Program.Id, (Observation.Id, Md5Hash, AsterismResults)] =
      sql"""
        SELECT
          c_observation_id,
          c_hash,
          c_asterism_results
        FROM t_itc_result
        WHERE c_program_id = $program_id
      """.query(observation_id *: md5_hash *: asterism_results)

    def selectAllItcResults[A <: NonEmptyList[Observation.Id]](enc: skunk.Encoder[A]): Query[A, (Observation.Id, Md5Hash, AsterismResults)] =
      sql"""
        SELECT
          c_observation_id,
          c_hash,
          c_asterism_results
        FROM t_itc_result
        WHERE c_observation_id IN ($enc)
      """.query(observation_id *: md5_hash *: asterism_results)

    val InsertOrUpdateItcResult: Command[(
      Program.Id,
      Observation.Id,
      Md5Hash,
      AsterismResults,
      Md5Hash,
      AsterismResults
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
          $asterism_results
        ON CONFLICT ON CONSTRAINT t_itc_result_pkey DO UPDATE
          SET c_hash             = $md5_hash,
              c_asterism_results = $asterism_results
      """.command

  }

}
