// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.logic

import cats.MonadThrow
import cats.Order
import cats.data.EitherNel
import cats.data.EitherT
import cats.data.NonEmptyList
import cats.syntax.applicativeError.*
import cats.syntax.either.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import eu.timepit.refined.types.numeric.NonNegInt
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.data.Zipper
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.itc.client.ItcClient
import lucuma.itc.client.SpectroscopyIntegrationTimeInput
import lucuma.itc.client.SpectroscopyResult
import lucuma.odb.sequence.data.GeneratorParams
import lucuma.odb.service.GeneratorParamsService


sealed trait Itc[F[_]] {

  def lookup(
    programId:     Program.Id,
    observationId: Observation.Id,
    useCache:      Boolean
  ): F[EitherNel[Itc.Error, Itc.ResultSet]]

  def spectroscopy(
    targets:  NonEmptyList[(Target.Id, SpectroscopyIntegrationTimeInput)],
    useCache: Boolean
  ): F[EitherNel[Itc.Error, Itc.ResultSet]]

}

object Itc {

  sealed trait Result

  sealed trait Error extends Result {
    def format: String
  }

  object Result {

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

    case class NoResult(
      targetId: Target.Id
    ) extends Error {
      def format: String =
        s"ITC returned no results for target '$targetId'."
    }

    case class ServiceError(
      targetId: Target.Id,
      message:  String
    ) extends Error {
      def format: String =
        s"ITC returned an error for target '$targetId': $message"
    }

    case class Success(
      targetId: Target.Id,
      input:    SpectroscopyIntegrationTimeInput,
      value:    IntegrationTime
    ) extends Result {
      def totalTime: Option[TimeSpan] = {
        val total = BigInt(value.exposureTime.toMicroseconds) * value.exposures.value
        Option.when(total.isValidLong)(TimeSpan.fromMicroseconds(total.longValue)).flatten
      }
    }

    object Success {

      given Order[Success] =
        Order.by { s => (s.totalTime, s.targetId) }

      import lucuma.odb.json.time.query.given

      given Encoder[Success] =
        Encoder.instance { s =>
          Json.obj(
            "targetId"      -> s.targetId.asJson,
            "exposureTime"  -> s.value.exposureTime.asJson,
            "exposures"     -> s.value.exposures.value.asJson,
            "signalToNoise" -> s.value.signalToNoise.asJson
          )
        }
    }
  }

  case class ResultSet(
    value: Zipper[Result.Success]
  )

  object ResultSet {

    given Encoder[ResultSet] =
      Encoder.instance { rs =>
        Json.obj(
          "result" -> rs.value.focus.asJson,
          "all"    -> rs.value.toList.asJson
        )
      }

  }

  def fromClientAndServices[F[_]: MonadThrow](
    client:    ItcClient[F],
    paramsSrv: GeneratorParamsService[F]
  ): Itc[F] =
    new Itc[F] {

      import Result.*

      override def lookup(
        programId:     Program.Id,
        observationId: Observation.Id,
        useCache:      Boolean
      ): F[EitherNel[Error, ResultSet]] =
        (for {
          params <- selectParams(programId, observationId).leftMap(NonEmptyList.one)
          result <- callItc(params, useCache)
        } yield result).value


      private def selectParams(
        pid: Program.Id,
        oid: Observation.Id
      ): EitherT[F, Error, GeneratorParams] =
        EitherT(
          paramsSrv
            .select(pid, oid)
            .map {
              case None                => ObservationNotFound(pid, oid).asLeft
              case Some(Left(missing)) => MissingParams(missing).asLeft
              case Some(Right(params)) => params.asRight
            }
        )

      private def callItc(
        params:   GeneratorParams,
        useCache: Boolean
      ): EitherT[F, NonEmptyList[Error], ResultSet] =

        params match {
          case GeneratorParams.GmosNorthLongSlit(itc, _) =>
            EitherT(spectroscopy(itc, useCache))

          case GeneratorParams.GmosSouthLongSlit(itc, _) =>
            EitherT(spectroscopy(itc, useCache))
        }


      override def spectroscopy(
        targets:  NonEmptyList[(Target.Id, SpectroscopyIntegrationTimeInput)],
        useCache: Boolean
      ): F[EitherNel[Error, ResultSet]] =
        targets.traverse { case (tid, si) =>
          client.spectroscopy(si, useCache).map {
            case SpectroscopyResult(_, None)                                 =>
              NoResult(tid).leftNel
            case SpectroscopyResult(_, Some(s @ IntegrationTime(_, _, _))) =>
              Success(tid, si, s).rightNel
          }
          .handleError { t =>
            ServiceError(tid, t.getMessage).leftNel
          }
        }.map(_.sequence.map(nel => ResultSet(Zipper.fromNel(nel).focusMax)))

    }

}
