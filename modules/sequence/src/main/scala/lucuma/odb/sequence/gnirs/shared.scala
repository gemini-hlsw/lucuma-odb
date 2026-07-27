// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.gnirs

import cats.syntax.eq.*
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsPixelScale
import lucuma.core.math.SignalToNoise
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan

// Definitions that are shared across GNIRS modes.
val MinAcquisitionExposureTime: TimeSpan = 100.msTimeSpan  // 0.1 s; actually determined by the read mode, but this is a reasonable lower bound for all modes.
val MaxAcquisitionExposureTime: TimeSpan = 60.secondTimeSpan

// The fixed, single-coadd exposure time for the initial "keyhole"/field image of an
// acquisition, as a function of the camera: 15s for the long cameras (0.05"/pix) and
// 3s for the short cameras (0.15"/pix). This is the H-band value the spectroscopy
// acquisition falls back to, reused as-is for imaging acquisitions.
def keyholeExposureTime(camera: GnirsCamera): TimeSpan =
  if camera.pixelScale === GnirsPixelScale.PixelScale_0_05 then 15.secTimeSpan else 3.secTimeSpan

// The fixed signal-to-noise used to classify the acquisition mode (Very Bright /
// Bright / Faint) from target brightness, independent of the user's requested S/N.
// See the two-pass acquisition ITC in ItcService.
val AcquisitionClassificationSignalToNoise: SignalToNoise =
  SignalToNoise.unsafeFromBigDecimalExact(BigDecimal(10))
