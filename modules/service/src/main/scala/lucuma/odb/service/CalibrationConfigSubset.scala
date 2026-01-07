// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import cats.syntax.option.*
import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2Fpu
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosBinning
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.ObservingModeType
import lucuma.core.math.Wavelength
import lucuma.odb.graphql.input.Flamingos2LongSlitInput
import lucuma.odb.graphql.input.GmosImagingFilterInput
import lucuma.odb.graphql.input.GmosImagingInput
import lucuma.odb.graphql.input.GmosImagingVariantInput
import lucuma.odb.graphql.input.GmosLongSlitInput
import lucuma.odb.graphql.input.ObservingModeInput
import lucuma.odb.sequence.ObservingMode
import lucuma.odb.sequence.flamingos2.longslit.Config as Flamingos2Config
import lucuma.odb.sequence.gmos.imaging.Config as ImagingConfig
import lucuma.odb.sequence.gmos.longslit.Config

sealed trait CalibrationConfigSubset derives Eq:
  def modeType: ObservingModeType

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
    def roi:               GmosRoi
    def modeType:          ObservingModeType

    def longSlitCommonInput: GmosLongSlitInput.Create.Common =
      GmosLongSlitInput.Create.Common(
        centralWavelength   = centralWavelength,
        exposureTimeMode    = none,
        explicitXBin        = xBin.some,
        explicitYBin        = yBin.some,
        explicitAmpReadMode = ampReadMode.some,
        explicitAmpGain     = ampGain.some,
        explicitRoi         = roi.some,
        explicitλDithers    = none,
        explicitOffsets     = none
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
    ampGain:           GmosAmpGain,
    roi:               GmosRoi
  ) extends Gmos[GmosNorthGrating, GmosNorthFilter, GmosNorthFpu] derives Eq:

    def modeType: ObservingModeType = ObservingModeType.GmosNorthLongSlit

    def toLongSlitInput: ObservingModeInput.Create =
      ObservingModeInput.Create(
        GmosLongSlitInput.Create.North(grating, filter, fpu, longSlitCommonInput, none).some,
        none,
        none,
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
    ampGain:           GmosAmpGain,
    roi:               GmosRoi
  ) extends Gmos[GmosSouthGrating, GmosSouthFilter, GmosSouthFpu] derives Eq:

    def modeType: ObservingModeType = ObservingModeType.GmosSouthLongSlit

    def toLongSlitInput: ObservingModeInput.Create =
      ObservingModeInput.Create(
        none,
        GmosLongSlitInput.Create.South(grating, filter, fpu, longSlitCommonInput, none).some,
        none,
        none,
        none
      )

  sealed trait GmosImaging[F] extends CalibrationConfigSubset:
    def filters:        NonEmptyList[F]
    def binning:        GmosBinning
    def ampReadMode:    GmosAmpReadMode
    def ampGain:        GmosAmpGain
    def roi:            GmosRoi

    def toImagingInput: ObservingModeInput.Create

  case class GmosNImagingConfigs(
    filters:     NonEmptyList[GmosNorthFilter],
    binning:     GmosBinning,
    ampReadMode: GmosAmpReadMode,
    ampGain:     GmosAmpGain,
    roi:         GmosRoi
  ) extends GmosImaging[GmosNorthFilter] derives Eq:

    def modeType: ObservingModeType = ObservingModeType.GmosNorthImaging

    def toImagingInput: ObservingModeInput.Create =
      ObservingModeInput.Create(
        none,
        none,
        GmosImagingInput.Create(
          GmosImagingVariantInput.Default,
          filters.map(f => GmosImagingFilterInput(f, none)),
          GmosImagingInput.Create.Common(
            binning.some,
            ampReadMode.some,
            ampGain.some,
            roi.some
          )
        ).some,
        none,
        none
      )

  case class GmosSImagingConfigs(
    filters:     NonEmptyList[GmosSouthFilter],
    binning:     GmosBinning,
    ampReadMode: GmosAmpReadMode,
    ampGain:     GmosAmpGain,
    roi:         GmosRoi
  ) extends GmosImaging[GmosSouthFilter] derives Eq:

    def modeType: ObservingModeType = ObservingModeType.GmosSouthImaging

    def toImagingInput: ObservingModeInput.Create =
      ObservingModeInput.Create(
        none,
        none,
        none,
        GmosImagingInput.Create(
          GmosImagingVariantInput.Default,
          filters.map(f => GmosImagingFilterInput(f, none)),
          GmosImagingInput.Create.Common(
            binning.some,
            ampReadMode.some,
            ampGain.some,
            roi.some
          )
        ).some,
        none
      )

  case class Flamingos2Configs(
    disperser: Flamingos2Disperser,
    filter:    Flamingos2Filter,
    fpu:       Flamingos2Fpu
  ) extends CalibrationConfigSubset derives Eq:

    def modeType: ObservingModeType = ObservingModeType.Flamingos2LongSlit

    def toLongSlitInput: ObservingModeInput.Create =
      ObservingModeInput.Create(
        none,
        none,
        none,
        none,
        Flamingos2LongSlitInput.Create(disperser, filter, fpu, none, none, none, none, none, none).some
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
            gn.ampGain,
            gn.roi
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
            gs.ampGain,
            gs.roi
          )
        case gni: ImagingConfig.GmosNorth =>
          GmosNImagingConfigs(
            gni.filters.map(_._1),
            gni.bin,
            gni.ampReadMode,
            gni.ampGain,
            gni.roi,
          )
        case gsi: ImagingConfig.GmosSouth =>
          GmosSImagingConfigs(
            gsi.filters.map(_._1),
            gsi.bin,
            gsi.ampReadMode,
            gsi.ampGain,
            gsi.roi
          )
        case f2: Flamingos2Config =>
          Flamingos2Configs(
            f2.disperser,
            f2.filter,
            f2.fpu
          )
