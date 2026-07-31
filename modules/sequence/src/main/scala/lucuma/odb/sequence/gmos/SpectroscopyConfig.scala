// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.gmos

import cats.Eq
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.math.Offset.Q
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDither
import lucuma.core.model.ExposureTimeMode

/**
 * The science-mode parameters shared by the GMOS slit-spectroscopy modes (long
 * slit and MOS).  These are the fields both modes carry verbatim; what varies is
 * the aperture (a builtin FPU for long slit, a custom mask for MOS) and the
 * acquisition configuration (present for long slit, absent for MOS), which stay
 * on each mode's own `Config`.
 *
 * MOS is calibrated, binned and estimated as a long slit, so the two are
 * expected to track each other closely; sharing `Common` keeps a change to the
 * shared fields from having to be made twice.
 */
object SpectroscopyConfig:

  final case class Common(
    centralWavelength:         Wavelength,
    exposureTimeMode:          ExposureTimeMode,
    defaultXBin:               GmosXBinning,
    explicitXBin:              Option[GmosXBinning],
    defaultYBin:               GmosYBinning,
    explicitYBin:              Option[GmosYBinning],
    explicitAmpReadMode:       Option[GmosAmpReadMode],
    explicitAmpGain:           Option[GmosAmpGain],
    explicitRoi:               Option[GmosRoi],
    explicitWavelengthDithers: Option[List[WavelengthDither]],
    explicitSpatialOffsets:    Option[List[Q]]
  )

  object Common:

    given Eq[Common] =
      Eq.by: a =>
        (
          a.centralWavelength,
          a.exposureTimeMode,
          a.defaultXBin,
          a.explicitXBin,
          a.defaultYBin,
          a.explicitYBin,
          a.explicitAmpReadMode,
          a.explicitAmpGain,
          a.explicitRoi,
          a.explicitWavelengthDithers,
          a.explicitSpatialOffsets
        )
