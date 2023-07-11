// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Order
import cats.data.EitherNel
import cats.data.EitherT
import cats.data.NonEmptyList
import cats.data.OptionT
import cats.effect.Async
import cats.effect.Concurrent
import cats.effect.Resource
import cats.effect.Temporal
import cats.effect.syntax.spawn.*
import cats.syntax.applicativeError.*
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import eu.timepit.refined.types.numeric.PosInt
import fs2.Stream
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.data.Zipper
import lucuma.core.math.SignalToNoise
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.itc.client.IntegrationTimeResult
import lucuma.itc.client.ItcClient
import lucuma.itc.client.SpectroscopyIntegrationTimeInput
import lucuma.odb.sequence.data.GeneratorParams
import lucuma.odb.service.NoTransaction
import lucuma.odb.service.Services.Syntax.*
import lucuma.odb.util.Codecs.*
import org.typelevel.log4cats.Logger
import skunk.*
import skunk.codec.text.text
import skunk.implicits.*

import java.security.MessageDigest
import java.util.HexFormat
import scala.concurrent.duration.*

sealed trait ItcService[F[_]] {

  def lookup(
    programId:     Program.Id,
    observationId: Observation.Id
  )(using NoTransaction[F]): F[Either[ItcService.Error, ItcService.Success]]

}

object ItcService {

  sealed trait Error {
    def format: String
  }

  object Error {

    case class ObservationNotFound(
      programId:     Program.Id,
      observationId: Observation.Id
    ) extends Error {
      def format: String =
        s"Observation '$observationId' in program '$programId' not found."
    }

    case class MissingParams(
      missing: NonEmptyList[GeneratorParamsService.MissingData]
    ) extends Error {
      def format: String = {
        val params = missing.map { m =>
          s"${m.targetId.fold("") { tid => s"(target $tid) " }}${m.paramName}"
        }.intercalate(", ")
        s"ITC cannot be queried until the following parameters are defined: $params"
      }
    }

    case class RemoteServiceErrors(
      problems: NonEmptyList[(Target.Id, String)]
    ) extends Error {
      def format: String = {
        val ps = problems.map { case (tid, msg) => s"Target '$tid': $msg"}
        s"ITC returned errors: ${ps.intercalate(", ")}"
      }
    }
  }

  case class TargetResult(
    targetId: Target.Id,
    input:    SpectroscopyIntegrationTimeInput,
    value:    IntegrationTime
  ) {
    def totalTime: Option[TimeSpan] = {
      val total = BigInt(value.exposureTime.toMicroseconds) * value.exposures.value
      Option.when(total.isValidLong)(TimeSpan.fromMicroseconds(total.longValue)).flatten
    }
  }

  object TargetResult {

    given Order[TargetResult] =
      Order.by { s => (s.totalTime, s.targetId) }

    import lucuma.odb.json.time.query.given

    given Encoder[TargetResult] =
      Encoder.instance { s =>
        Json.obj(
          "targetId"      -> s.targetId.asJson,
          "exposureTime"  -> s.value.exposureTime.asJson,
          "exposures"     -> s.value.exposures.value.asJson,
          "signalToNoise" -> s.value.signalToNoise.asJson
        )
      }
  }

  case class AsterismResult(
    value: Zipper[TargetResult]
  )

  object AsterismResult {

    given Encoder[AsterismResult] =
      Encoder.instance { rs =>
        Json.obj(
          "result" -> rs.value.focus.asJson,
          "all"    -> rs.value.toList.asJson
        )
      }

  }

  case class Success(
    params: GeneratorParams,
    result: AsterismResult
  )

  private val hex = HexFormat.of

  private def hash(input: SpectroscopyIntegrationTimeInput): String =
    hex.formatHex(
      MessageDigest
        .getInstance("MD5")
        .digest(input.asJson.noSpaces.getBytes("UTF-8"))
    )

  def pollVersionsForever[F[_]: Async: Temporal: Logger](
    client:     Resource[F, ItcClient[F]],
    session:    Resource[F, Session[F]],
    pollPeriod: FiniteDuration
  ): F[Unit] = {
    val pollOnce: F[Unit] =
      for {
        v <- client.use(_.versions)
        _ <- session.use { s => s.transaction.use(_ => s.execute(Statements.UpdateItcVersion)(v.server.some, v.data)) }
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

  def instantiate[F[_]: Concurrent](client: ItcClient[F])(using Services[F]): ItcService[F] =
    new ItcService[F] {

      import Error.*

      override def lookup(
        pid: Program.Id,
        oid: Observation.Id
      )(using NoTransaction[F]): F[Either[Error, Success]] =
        (for {
          pr     <- EitherT(attemptLookup(pid, oid))
          (params, storedResult) = pr
          result <- storedResult.fold(EitherT(callAndInsert(pid, oid, params)))(r => EitherT.pure(r))
        } yield Success(params, result)).value

      // Selects the parameters then selects the previously stored result set, if any.
      private def attemptLookup(
        pid: Program.Id,
        oid: Observation.Id
      )(using NoTransaction[F]): F[Either[Error, (GeneratorParams, Option[AsterismResult])]] =
        services.transactionally {
          (for {
            p <- EitherT(selectParams(pid, oid))
            r <- EitherT.liftF(selectResult(pid, oid, p))
          } yield (p, r)).value
        }

      private def selectParams(
        pid: Program.Id,
        oid: Observation.Id
      )(using Transaction[F]): F[Either[Error, GeneratorParams]] =
        generatorParamsService
          .select(pid, oid)
          .map {
            case None                => ObservationNotFound(pid, oid).asLeft
            case Some(Left(missing)) => MissingParams(missing).asLeft
            case Some(Right(params)) => params.asRight
          }

      // Selects the asterism result as a whole by selecting all the individual
      // target results.  If any individual result is not found then the asterism
      // as a whole is considered not found.
      private def selectResult(
        pid:    Program.Id,
        oid:    Observation.Id,
        params: GeneratorParams
      )(using Transaction[F]): F[Option[AsterismResult]] = {

        def selectSingleTarget(
          tid:   Target.Id,
          input: SpectroscopyIntegrationTimeInput
        ): F[Option[TargetResult]] =
          session
            .option(Statements.SelectItcResult)(pid, oid, tid)
            .map(_.collect { case (h, time) if h === hash(input) =>
              TargetResult(tid, input, time)
            })

        val specs = params match {
          case GeneratorParams.GmosNorthLongSlit(specs, _) => specs
          case GeneratorParams.GmosSouthLongSlit(specs, _) => specs
        }

        specs
          .traverse { case (tid, input) => OptionT(selectSingleTarget(tid, input)) }
          .value
          .map(_.map(lst => AsterismResult(Zipper.fromNel(lst).focusMax)))
      }

      // Calls the remote ITC service and stores the results in a table for
      // future lookups.
      private def callAndInsert(
        pid:      Program.Id,
        oid:      Observation.Id,
        params:   GeneratorParams
      ): F[Either[Error, AsterismResult]] =
        (for {
          r <- EitherT(callRemote(params))
          _ <- EitherT.liftF(services.transactionally(insertOrUpdate(pid, oid, r)))
        } yield r).value

      private def callRemote(
        params:   GeneratorParams
      )(using NoTransaction[F]): F[Either[Error, AsterismResult]] =
        params match {
          case GeneratorParams.GmosNorthLongSlit(itc, _) => callRemoteSpectroscopy(itc)
          case GeneratorParams.GmosSouthLongSlit(itc, _) => callRemoteSpectroscopy(itc)
        }

      private def callRemoteSpectroscopy(
        targets:  NonEmptyList[(Target.Id, SpectroscopyIntegrationTimeInput)]
      )(using NoTransaction[F]): F[Either[Error, AsterismResult]] =
        targets.traverse { case (tid, si) =>
          client.spectroscopy(si, useCache = false).map {
            case IntegrationTimeResult(_, results) =>
              TargetResult(tid, si, results.head).rightNel
          }
          .handleError { t => (tid, t.getMessage).leftNel }
        }.map(_.sequence.bimap(
          errors  => RemoteServiceErrors(errors),
          targets => AsterismResult(Zipper.fromNel(targets).focusMax)
        ))

      private def insertOrUpdate(
        pid:       Program.Id,
        oid:       Observation.Id,
        resultSet: AsterismResult
      )(using Transaction[F]): F[Unit] = {

        def insertOrUpdateSingleTarget(success: TargetResult): F[Unit] = {
          val h = hash(success.input)
          session.execute(Statements.InsertOrUpdateItcResult)(
            pid,
            oid,
            success.targetId,
            h,
            success.value.exposureTime,
            success.value.exposures,
            success.value.signalToNoise,
            h,
            success.value.exposureTime,
            success.value.exposures,
            success.value.signalToNoise,
          ).void
        }

        resultSet.value.traverse(insertOrUpdateSingleTarget).void
      }

    }

  object Statements {
    private val integration_time: Codec[IntegrationTime] =
      (time_span *: pos_int *: signal_to_noise).to[IntegrationTime]

    val UpdateItcVersion: Command[(
      Option[String],
      Option[String]
    )] =
      sql"""
        UPDATE t_itc_version
           SET c_version = ${text.opt},
               c_data    = ${text.opt}
      """.command

    val SelectItcResult: Query[(
      Program.Id,
      Observation.Id,
      Target.Id
    ), (String, IntegrationTime)] =
      sql"""
        SELECT
          c_hash,
          c_exposure_time,
          c_exposure_count,
          c_signal_to_noise
        FROM t_itc_result
        WHERE c_program_id     = $program_id     AND
              c_observation_id = $observation_id AND
              c_target_id      = $target_id
      """.query(text *: integration_time)

    val InsertOrUpdateItcResult: Command[(
      Program.Id,
      Observation.Id,
      Target.Id,
      String,
      TimeSpan,
      PosInt,
      SignalToNoise,
      String,
      TimeSpan,
      PosInt,
      SignalToNoise
    )] =
      sql"""
        INSERT INTO t_itc_result (
          c_program_id,
          c_observation_id,
          c_target_id,
          c_hash,
          c_exposure_time,
          c_exposure_count,
          c_signal_to_noise
        ) SELECT
          $program_id,
          $observation_id,
          $target_id,
          $text,
          $time_span,
          $pos_int,
          $signal_to_noise
        ON CONFLICT ON CONSTRAINT t_itc_result_pkey DO UPDATE
          SET c_hash            = $text,
              c_exposure_time   = $time_span,
              c_exposure_count  = $pos_int,
              c_signal_to_noise = $signal_to_noise
      """.command


  }

}