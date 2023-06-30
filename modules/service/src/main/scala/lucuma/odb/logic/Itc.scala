// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.logic

import cats.MonadThrow
import cats.Order
import cats.data.EitherNel
import cats.data.NonEmptyList
import cats.syntax.applicativeError.*
import cats.syntax.either.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.data.Zipper
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.itc.client.IntegrationTimeResult
import lucuma.itc.client.ItcClient
import lucuma.itc.client.SpectroscopyIntegrationTimeInput
import lucuma.odb.sequence.data.GeneratorParams
import lucuma.odb.service.GeneratorParamsService
import lucuma.odb.service.NoTransaction
import lucuma.odb.service.Services
import lucuma.odb.service.Services.Syntax.*
import skunk.Transaction

sealed trait Itc[F[_]] {

  def selectParams(
    pid: Program.Id,
    oid: Observation.Id
  )(using Transaction[F]): F[EitherNel[Itc.Error, GeneratorParams]]

  def callService(
    params:   GeneratorParams,
    useCache: Boolean
  )(using NoTransaction[F]): F[EitherNel[Itc.Error, Itc.ResultSet]]

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

  def instantiate[F[_]: MonadThrow](client: ItcClient[F])(using Services[F]): Itc[F] =
    new Itc[F] {

      import Result.*

      override def selectParams(
        pid: Program.Id,
        oid: Observation.Id
      )(using Transaction[F]): F[EitherNel[Error, GeneratorParams]] =
        generatorParamsService
          .select(pid, oid)
          .map {
            case None                => NonEmptyList.one(ObservationNotFound(pid, oid)).asLeft
            case Some(Left(missing)) => NonEmptyList.one(MissingParams(missing)).asLeft
            case Some(Right(params)) => params.asRight
          }

      override def callService(
        params:   GeneratorParams,
        useCache: Boolean
      )(using NoTransaction[F]): F[EitherNel[Error, ResultSet]] =
        params match {
          case GeneratorParams.GmosNorthLongSlit(itc, _) => spectroscopy(itc, useCache)
          case GeneratorParams.GmosSouthLongSlit(itc, _) => spectroscopy(itc, useCache)
        }

      private def spectroscopy(
        targets:  NonEmptyList[(Target.Id, SpectroscopyIntegrationTimeInput)],
        useCache: Boolean
      ): F[EitherNel[Error, ResultSet]] =
        targets.traverse { case (tid, si) =>
          client.spectroscopy(si, useCache).map {
            case IntegrationTimeResult(_, results) =>
              Success(tid, si, results.head).rightNel
          }
          .handleError { t =>
            ServiceError(tid, t.getMessage).leftNel
          }
        }.map(_.sequence.map(nel => ResultSet(Zipper.fromNel(nel).focusMax)))

    }

}
