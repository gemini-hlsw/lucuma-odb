// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.Order
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.data.Zipper
import lucuma.core.data.ZipperCodec.given
import lucuma.core.enums.Band
import lucuma.core.math.Wavelength
import lucuma.core.util.TimeSpan

case class TargetIntegrationTime(
  times:           Zipper[IntegrationTime],
  bandOrLine:      Either[Band, Wavelength],
  signalToNoiseAt: Option[SignalToNoiseAt],
  ccds:            List[ItcCcd]
):
  def focusIndex(index: Int): Option[TargetIntegrationTime] =
    times.focusIndex(index).map(newTimes => copy(times = newTimes))

object TargetIntegrationTime:
  given (using
    Encoder[Wavelength],
    Encoder[TimeSpan]
  ): Encoder[TargetIntegrationTime] = t =>
    val common = Json
      .obj(
        "band"            -> t.bandOrLine.left.toOption.asJson,
        "emissionLine"    -> t.bandOrLine.toOption.asJson,
        "signalToNoiseAt" -> t.signalToNoiseAt.asJson,
        "ccds"            -> t.ccds.asJson
      )
    // Merge common fields with times fields which are now guaranteed to be present
    t.times.asJson.deepMerge(common)

  given Order[TargetIntegrationTime] = Order.by(_.times.focus)
