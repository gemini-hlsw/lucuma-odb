// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Eq
import cats.derived.*
import cats.syntax.option.*
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.math.Wavelength
import lucuma.odb.graphql.input.GmosLongSlitInput
import lucuma.odb.graphql.input.ObservingModeInput
import lucuma.odb.sequence.ObservingMode
import lucuma.odb.sequence.f2.longslit.Config as F2Config
import lucuma.odb.sequence.gmos.longslit.Config

sealed trait CalibrationConfigSubset derives Eq

object CalibrationConfigSubset:
  sealed trait Gmos[G, L, U] extends CalibrationConfigSubset:
    def grating:           G
    def filter:            Option[L]
    def fpu:               U
    def centralWavelength: Wavelength
    def xBin:              GmosXBinning
    def yBin:              GmosYBinning
    def ampReadMode:       GmosAmpReadMode
    def ampGain:           GmosAmpGain

    def longSlitCommonInput: GmosLongSlitInput.Create.Common =
      GmosLongSlitInput.Create.Common(
        centralWavelength      = centralWavelength,
        explicitXBin           = xBin.some,
        explicitYBin           = yBin.some,
        explicitAmpReadMode    = ampReadMode.some,
        explicitAmpGain        = ampGain.some,
        explicitRoi            = GmosRoi.CentralSpectrum.some,
        explicitλDithers       = none,
        explicitSpatialOffsets = none
      )

    def toLongSlitInput: ObservingModeInput.Create

  case class GmosNConfigs(
    grating:           GmosNorthGrating,
    filter:            Option[GmosNorthFilter],
    fpu:               GmosNorthFpu,
    centralWavelength: Wavelength,
    xBin:              GmosXBinning,
    yBin:              GmosYBinning,
    ampReadMode:       GmosAmpReadMode,
    ampGain:           GmosAmpGain
  ) extends Gmos[GmosNorthGrating, GmosNorthFilter, GmosNorthFpu] derives Eq:

    def toLongSlitInput: ObservingModeInput.Create =
      ObservingModeInput.Create(
        GmosLongSlitInput.Create.North(grating, filter, fpu, longSlitCommonInput).some,
        none,
        none
      )

  case class GmosSConfigs(
    grating:           GmosSouthGrating,
    filter:            Option[GmosSouthFilter],
    fpu:               GmosSouthFpu,
    centralWavelength: Wavelength,
    xBin:              GmosXBinning,
    yBin:              GmosYBinning,
    ampReadMode:       GmosAmpReadMode,
    ampGain:           GmosAmpGain
  ) extends Gmos[GmosSouthGrating, GmosSouthFilter, GmosSouthFpu] derives Eq:

    def toLongSlitInput: ObservingModeInput.Create =
      ObservingModeInput.Create(
        none,
        GmosLongSlitInput.Create.South(grating, filter, fpu, longSlitCommonInput).some,
        none
      )

  extension (mode: ObservingMode)
    def toConfigSubset: CalibrationConfigSubset =
      mode match
        case gn: Config.GmosNorth =>
          GmosNConfigs(
            gn.grating,
            gn.filter,
            gn.fpu,
            gn.centralWavelength,
            gn.xBin,
            gn.yBin,
            gn.ampReadMode,
            gn.ampGain
          )
        case gs: Config.GmosSouth =>
          GmosSConfigs(
            gs.grating,
            gs.filter,
            gs.fpu,
            gs.centralWavelength,
            gs.xBin,
            gs.yBin,
            gs.ampReadMode,
            gs.ampGain
          )
        case f2: F2Config =>
          ???
