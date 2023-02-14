// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import cats.Applicative
import cats.Order
import cats.data.EitherNel
import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import eu.timepit.refined.types.numeric.NonNegInt
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax._
import lucuma.core.data.Zipper
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.util.TimeSpan
import lucuma.itc.client.ItcClient
import lucuma.itc.client.ItcResult
import lucuma.itc.client.SpectroscopyModeInput
import lucuma.itc.client.SpectroscopyResult
import lucuma.odb.sequence.data.GeneratorParams


sealed trait Itc[F[_]] {

  def lookup(
    params:   GeneratorParams,
    useCache: Boolean
  ): F[EitherNel[Itc.Error, Itc.ResultSet]]

  def spectroscopy(
    targets:  NonEmptyList[(Target.Id, SpectroscopyModeInput)],
    useCache: Boolean
  ): F[EitherNel[Itc.Error, Itc.ResultSet]]

}

object Itc {

  case class Error(
    targetId: Target.Id,
    message:  String
  )

  case class Success(
    targetId: Target.Id,
    input:    SpectroscopyModeInput,
    value:    ItcResult.Success
  ) {
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
          "signalToNoise" -> s.value.signalToNoise.value.asJson
        )
      }
  }

  case class ResultSet(
    value: Zipper[Success]
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

  def fromClient[F[_]: Applicative](
    client: ItcClient[F]
  ): Itc[F] =
    new Itc[F] {

      override def lookup(
        params:   GeneratorParams,
        useCache: Boolean
      ): F[EitherNel[Itc.Error, Itc.ResultSet]] =

        params match {

          case GeneratorParams.GmosNorthLongSlit(itc, _) =>
            spectroscopy(itc, useCache)

          case GeneratorParams.GmosSouthLongSlit(itc, _) =>
            spectroscopy(itc, useCache)
        }

      override def spectroscopy(
        targets:  NonEmptyList[(Target.Id, SpectroscopyModeInput)],
        useCache: Boolean
      ): F[EitherNel[Itc.Error, Itc.ResultSet]] =
        targets.traverse { case (tid, si) =>
          client.spectroscopy(si, useCache).map {
            case SpectroscopyResult(_, None)                                 =>
              Error(tid, "ITC service returned nothing.").leftNel
            case SpectroscopyResult(_, Some(ItcResult.Error(msg)))           =>
              Error(tid, msg).leftNel
            case SpectroscopyResult(_, Some(s @ ItcResult.Success(_, _, _))) =>
              Success(tid, si, s).rightNel
          }
        }.map(_.sequence.map(nel => ResultSet(Zipper.fromNel(nel).focusMax)))

    }

}
