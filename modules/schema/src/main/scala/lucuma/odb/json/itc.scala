// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import eu.timepit.refined.types.numeric.PosInt
import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.refined.*
import io.circe.syntax.*
import lucuma.core.data.Zipper
import lucuma.core.data.ZipperCodec
import lucuma.core.math.SignalToNoise
import lucuma.core.math.SingleSN
import lucuma.core.math.TotalSN
import lucuma.core.math.Wavelength
import lucuma.core.model.Target
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.itc.SignalToNoiseAt
import lucuma.odb.data.Itc
import lucuma.odb.data.Itc.Result
import lucuma.odb.data.Itc.ResultSet

trait ItcCodec:

  import time.decoder.given
  import wavelength.decoder.given
  import ZipperCodec.given

  given Decoder[SignalToNoiseAt] = c =>
    for {
      w <- c.downField("wavelength").as[Wavelength]
      s <- c.downField("single").as[SignalToNoise]
      t <- c.downField("total").as[SignalToNoise]
    } yield SignalToNoiseAt(w, SingleSN(s), TotalSN(t))

  // N.B. lucuma.itc.SignalToNoiseAt defines its own encoder consistent with
  // this decoder.  Perhaps we should move the decoder there as well.

  given Decoder[Result] =
    Decoder.instance: c =>
      for
        targetId        <- c.downField("targetId").as[Target.Id]
        exposureTime    <- c.downField("exposureTime").as[TimeSpan]
        exposureCount   <- c.downField("exposureCount").as[PosInt]
        signalToNoiseAt <- c.downField("signalToNoiseAt").as[Option[SignalToNoiseAt]]
      yield Result(targetId, IntegrationTime(exposureTime, exposureCount), signalToNoiseAt)

  given (using Encoder[TimeSpan], Encoder[Wavelength]): Encoder[Result] =
    Encoder.instance: a =>
      Json.obj(
        "targetId"        -> a.targetId.asJson,
        "exposureTime"    -> a.value.exposureTime.asJson,
        "exposureCount"   -> a.value.exposureCount.value.asJson,
        "signalToNoiseAt" -> a.signalToNoise.asJson
      )

  given Decoder[ResultSet] =
    Decoder.instance: c =>
      c.as[Zipper[Result]].map(z => ResultSet(z))

  given (using Encoder[TimeSpan], Encoder[Wavelength]): Encoder[ResultSet] =
    Encoder.instance: a =>
      a.results.asJson

  given Decoder[Itc] =
    Decoder.instance: c =>
      for
        acquisition <- c.downField("acquisition").as[Option[ResultSet]]
        science     <- c.downField("science").as[ResultSet]
      yield Itc(acquisition, science)

  given (using Encoder[TimeSpan], Encoder[Wavelength]): Encoder[Itc] =
    Encoder.instance: a =>
      Json.obj(
        "acquisition" -> a.acquisition.asJson,
        "science"     -> a.science.asJson
      )

object itc extends ItcCodec