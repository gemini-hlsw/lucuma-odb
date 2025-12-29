// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import cats.Order
import cats.data.NonEmptyList
import cats.data.NonEmptyMap
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.Json
import io.circe.refined.*
import io.circe.syntax.*
import lucuma.core.data.Zipper
import lucuma.core.data.ZipperCodec
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.math.SignalToNoise
import lucuma.core.math.SingleSN
import lucuma.core.math.TotalSN
import lucuma.core.math.Wavelength
import lucuma.core.model.Target
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.itc.SignalToNoiseAt
import lucuma.odb.data.Itc

trait ItcCodec:

  import time.decoder.given
  import wavelength.decoder.given
  import ZipperCodec.given

  given Decoder[SignalToNoiseAt] = c =>
    for
      w <- c.downField("wavelength").as[Wavelength]
      s <- c.downField("single").as[SignalToNoise]
      t <- c.downField("total").as[SignalToNoise]
    yield SignalToNoiseAt(w, SingleSN(s), TotalSN(t))

  // N.B. lucuma.itc.SignalToNoiseAt defines its own encoder consistent with
  // this decoder.  Perhaps we should move the decoder there as well.

  given Decoder[Itc.Result] =
    Decoder.instance: c =>
      for
        targetId        <- c.downField("targetId").as[Target.Id]
        exposureTime    <- c.downField("exposureTime").as[TimeSpan]
        exposureCount   <- c.downField("exposureCount").as[PosInt]
        signalToNoiseAt <- c.downField("signalToNoiseAt").as[Option[SignalToNoiseAt]]
      yield Itc.Result(targetId, IntegrationTime(exposureTime, exposureCount), signalToNoiseAt)

  given (using Encoder[TimeSpan], Encoder[Wavelength]): Encoder[Itc.Result] =
    Encoder.instance: a =>
      Json.obj(
        "targetId"        -> a.targetId.asJson,
        "exposureTime"    -> a.value.exposureTime.asJson,
        "exposureCount"   -> a.value.exposureCount.value.asJson,
        "signalToNoiseAt" -> a.signalToNoise.asJson
      )

  private def imagingScienceNemDecoder[A: Decoder: Order](
    fieldName: String
  ): Decoder[NonEmptyMap[A, Zipper[Itc.Result]]] =
    Decoder.instance: c =>
      c.downField(fieldName)
       .values
       .flatMap(it => NonEmptyList.fromList(it.toList))
       .toRight(DecodingFailure("Expecting at least one ITC result set.", c.history))
       .flatMap: nel =>
         val res = nel.traverse: json =>
           val c = json.hcursor
           for
             filter  <- c.downField("filter").as[A]
             results <- c.downField("results").as[Zipper[Itc.Result]]
           yield filter -> results
         res.map(_.toNem)

  given Decoder[Itc.GmosNorthImaging] =
    imagingScienceNemDecoder[GmosNorthFilter]("gmosNorthImagingScience").map(Itc.GmosNorthImaging.apply)

  given Decoder[Itc.GmosSouthImaging] =
    imagingScienceNemDecoder[GmosSouthFilter]("gmosSouthImagingScience").map(Itc.GmosSouthImaging.apply)

  given Decoder[Itc.Spectroscopy] =
    Decoder.instance: c =>
      for
        acquisition <- c.downField("acquisition").as[Zipper[Itc.Result]]
        science     <- c.downField("spectroscopyScience").as[Zipper[Itc.Result]]
      yield Itc.Spectroscopy(acquisition, science)

  private def imagingScienceNemEncoder[A: Encoder](
    using Encoder[TimeSpan], Encoder[Wavelength]
  ): Encoder[NonEmptyMap[A, Zipper[Itc.Result]]] =
    Encoder.instance: a =>
      Json.fromValues:
        a.toNel.toList.map: (filter, results) =>
          Json.obj(
            "filter"  -> filter.asJson,
            "results" -> results.asJson
          )

  given (using Encoder[TimeSpan], Encoder[Wavelength]): Encoder[Itc.GmosNorthImaging] =
    Encoder.instance: a =>
      Json.obj(
        "itcType"                 -> Itc.Type.GmosNorthImaging.asJson,
        "gmosNorthImagingScience" -> a.science.asJson(using imagingScienceNemEncoder[GmosNorthFilter])
      )

  given (using Encoder[TimeSpan], Encoder[Wavelength]): Encoder[Itc.GmosSouthImaging] =
    Encoder.instance: a =>
      Json.obj(
        "itcType"                 -> Itc.Type.GmosSouthImaging.asJson,
        "gmosSouthImagingScience" -> a.science.asJson(using imagingScienceNemEncoder[GmosSouthFilter])
      )

  given (using Encoder[TimeSpan], Encoder[Wavelength]): Encoder[Itc.Spectroscopy] =
    Encoder.instance: a =>
      Json.obj(
        "itcType"             -> Itc.Type.Spectroscopy.asJson,
        "acquisition"         -> a.acquisition.asJson,
        "spectroscopyScience" -> a.science.asJson
      )

  given Decoder[Itc] =
    Decoder.instance: c =>
      c.downField("itcType")
       .as[Itc.Type]
       .flatMap:
         case Itc.Type.GmosNorthImaging => Decoder[Itc.GmosNorthImaging].apply(c)
         case Itc.Type.GmosSouthImaging => Decoder[Itc.GmosSouthImaging].apply(c)
         case Itc.Type.Spectroscopy     => Decoder[Itc.Spectroscopy].apply(c)

  given (using Encoder[TimeSpan], Encoder[Wavelength]): Encoder[Itc] =
    Encoder.instance:
      case a @ Itc.GmosNorthImaging(_) => Encoder[Itc.GmosNorthImaging].apply(a)
      case a @ Itc.GmosSouthImaging(_) => Encoder[Itc.GmosSouthImaging].apply(a)
      case a @ Itc.Spectroscopy(_, _)  => Encoder[Itc.Spectroscopy].apply(a)

object itc extends ItcCodec