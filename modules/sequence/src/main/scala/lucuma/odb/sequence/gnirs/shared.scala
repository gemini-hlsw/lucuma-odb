// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.gnirs

import cats.syntax.eq.*
import lucuma.core.enums.GnirsAcquisitionType
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsPixelScale
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
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
//
// By construction this equals `acquisitionSignalToNoise(Faint)`, which is why a
// target that classifies as Faint needs no second ITC pass: the classification
// call already computed its exposure at the S/N the acquisition will use.
val AcquisitionClassificationSignalToNoise: SignalToNoise =
  acquisitionSignalToNoise(GnirsAcquisitionType.Faint)

// The signal-to-noise an automatic acquisition targets, as a function of the
// brightness classification: brighter targets are acquired at a higher S/N because
// the exposure needed to reach it is still short.
//
// ATTENTION: duplicated in the migration that backfills c_is_explicit. Modify in sync.
def acquisitionSignalToNoise(acquisitionType: GnirsAcquisitionType): SignalToNoise =
  val sn = acquisitionType match
    case GnirsAcquisitionType.VeryBright => 30
    case GnirsAcquisitionType.Bright     => 20
    case GnirsAcquisitionType.Faint      => 10
  SignalToNoise.unsafeFromBigDecimalExact(BigDecimal(sn))

// The acquisition exposure time mode to use when the user has not set one: always
// signal-to-noise, at the value the brightness classification calls for. An explicit
// acquisition type determines the classification outright; otherwise we start from Faint
// and the ITC rewrites it once it has classified the target.
def derivedAcquisitionExposureTimeMode(
  acquisitionType: Option[GnirsAcquisitionType],
  at:              Wavelength
): ExposureTimeMode =
  ExposureTimeMode.SignalToNoiseMode(
    acquisitionSignalToNoise(acquisitionType.getOrElse(GnirsAcquisitionType.Faint)),
    at
  )
