// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import io.circe.Decoder
import lucuma.itc.ItcWarning

case class ItcRemoteCcd(
  singleSNRatio: Double,          // the final SN ratio for a single image
  totalSNRatio:  Double,          // the total SN ratio for all images
  peakPixelFlux: Double,          // the highest e- count for all pixels on the CCD
  wellDepth:     Double,          // the well depth (max e- count per pixel) for this CCD
  ampGain:       Double,          // the amplifier gain for this CCD (used to calculate ADU)
  warnings:      List[ItcWarning] // the warnings provided by ITC for this CCD
) derives Decoder
