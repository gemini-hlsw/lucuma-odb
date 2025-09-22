// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.Eq
import cats.derived.*
import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.generic.semiauto.*
import io.circe.syntax.*
import lucuma.core.math.SignalToNoise
import lucuma.core.math.SingleSN
import lucuma.core.math.TotalSN
import lucuma.core.math.Wavelength

case class ItcCcd(
  singleSNRatio:                 SingleSN,           // the final SN ratio for a single image
  maxSingleSNRatio:              Option[Double],     // the max single SN ratio for a single image/ccd
  totalSNRatio:                  TotalSN,            // the total SN ratio for all images
  maxTotalSNRatio:               Option[Double],     // the max final SN ratio for all images/ccd
  wavelengthForMaxTotalSNRatio:  Option[Wavelength], // Wavelength where we get the max total SN
  wavelengthForMaxSingleSNRatio: Option[Wavelength], // Wavelength where we get the max single SN
  peakPixelFlux:                 Double,             // the highest e- count for all pixels on the CCD
  wellDepth:                     Double,             // the well depth (max e- count per pixel) for this CCD
  ampGain:                       Double,             // the amplifier gain for this CCD (used to calculate ADU)
  warnings:                      List[ItcWarning]    // the warnings provided by ITC for this CCD
) derives Eq:

  // the max percentage of the well saturation for peak pixel
  val percentFullWell: Double =
    peakPixelFlux / wellDepth * 100.0

  // the ADU value
  val adu: Int =
    (peakPixelFlux / ampGain).toInt

object ItcCcd:
  given (using Encoder[Wavelength], Encoder[SignalToNoise]): Encoder[ItcCcd] = c =>
    Json.obj(
      "singleSNRatio"                 -> c.singleSNRatio.asJson,
      "maxSingleSNRatio"              -> c.maxSingleSNRatio.asJson,
      "totalSNRatio"                  -> c.totalSNRatio.asJson,
      "maxTotalSNRatio"               -> c.maxTotalSNRatio.asJson,
      "wavelengthForMaxSingleSNRatio" -> c.wavelengthForMaxSingleSNRatio.asJson,
      "wavelengthForMaxTotalSNRatio"  -> c.wavelengthForMaxTotalSNRatio.asJson,
      "peakPixelFlux"                 -> c.peakPixelFlux.asJson,
      "wellDepth"                     -> c.wellDepth.asJson,
      "ampGain"                       -> c.ampGain.asJson,
      "warnings"                      -> c.warnings.asJson
    )

  given (using Decoder[Wavelength], Decoder[SignalToNoise]): Decoder[ItcCcd] = deriveDecoder
