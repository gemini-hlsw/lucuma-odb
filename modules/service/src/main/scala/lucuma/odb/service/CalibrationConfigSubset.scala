// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import cats.syntax.eq.*
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
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsPrism
import lucuma.core.enums.ObservingModeType
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.gnirs.GnirsFpu
import lucuma.odb.graphql.input.Flamingos2LongSlitInput
import lucuma.odb.graphql.input.GmosImagingFilterInput
import lucuma.odb.graphql.input.GmosImagingInput
import lucuma.odb.graphql.input.GmosLongSlitInput
import lucuma.odb.graphql.input.ImagingVariantInput
import lucuma.odb.graphql.input.ObservingModeInput
import lucuma.odb.sequence.ObservingMode
import lucuma.odb.sequence.exchange.Config as ExchangeConfig
import lucuma.odb.sequence.flamingos2.imaging.Config as Flamingos2ImagingConfig
import lucuma.odb.sequence.flamingos2.longslit.Config as Flamingos2Config
import lucuma.odb.sequence.flamingos2.mos.Config as Flamingos2MosConfig
import lucuma.odb.sequence.ghost.ifu.Config as GhostConfig
import lucuma.odb.sequence.gmos.ifu.Config as IfuConfig
import lucuma.odb.sequence.gmos.imaging.Config as ImagingConfig
import lucuma.odb.sequence.gmos.longslit.Config
import lucuma.odb.sequence.gmos.mos.Config as MosConfig
import lucuma.odb.sequence.gnirs.imaging.Config as GnirsImagingConfig
import lucuma.odb.sequence.gnirs.spectroscopy.Config as GnirsSpectroscopyConfig
import lucuma.odb.sequence.igrins2.longslit.Config as Igrins2Config
import lucuma.odb.sequence.visitor.Config as VisitorConfig

sealed trait CalibrationConfigSubset derives Eq:
  def modeType: ObservingModeType

object CalibrationConfigSubset:

  case class ExchangeConfigSubset(config: ExchangeConfig) extends CalibrationConfigSubset:
    def modeType: ObservingModeType = config.mode

  case class VisitorConfigSubset(config: VisitorConfig) extends CalibrationConfigSubset:
    def modeType: ObservingModeType = config.mode

  // TODO: What do we need here?
  case object GhostConfigs extends CalibrationConfigSubset derives Eq:
    def modeType: ObservingModeType = ObservingModeType.GhostIfu

  case class GnirsSpectroscopyConfigs(config: GnirsSpectroscopyConfig) extends CalibrationConfigSubset derives Eq:
    def modeType: ObservingModeType =
      config.fpu match
        case _: GnirsFpu.Spectroscopy.Slit => ObservingModeType.GnirsLongSlit
        case _: GnirsFpu.Spectroscopy.Ifu  => ObservingModeType.GnirsIfu

    /** Cross-dispersed configurations use the SXD or LXD prism (cross-disperser). */
    def isCrossDispersed: Boolean =
      config.prism === GnirsPrism.Sxd || config.prism === GnirsPrism.Lxd

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
        centralWavelength        = centralWavelength,
        exposureTimeMode         = none,
        explicitXBin             = xBin.some,
        explicitYBin             = yBin.some,
        explicitAmpReadMode      = ampReadMode.some,
        explicitAmpGain          = ampGain.some,
        explicitRoi              = roi.some,
        explicitλDithers         = none,
        explicitTelescopeConfigs = none
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
        none,
        none,
        none,
        none,
        none,
        none,
        none,
        GmosLongSlitInput.Create.North(grating, filter, fpu, longSlitCommonInput, none).some,
        none,
        none,
        none,
        none,
        none,
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
        none,
        none,
        none,
        none,
        none,
        none,
        none,
        none,
        none,
        none,
        GmosLongSlitInput.Create.South(grating, filter, fpu, longSlitCommonInput, none).some,
        none,
        none,
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
        none,
        none,
        none,
        none,
        GmosImagingInput.Create(
          ImagingVariantInput.Default,
          filters.map(f => GmosImagingFilterInput(f, none)),
          GmosImagingInput.Create.Common(
            binning.some,
            ampReadMode.some,
            ampGain.some,
            roi.some
          )
        ).some,
        none,
        none,
        none,
        none,
        none,
        none,
        none,
        none,
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
        none,
        none,
        none,
        none,
        none,
        none,
        none,
        GmosImagingInput.Create(
          ImagingVariantInput.Default,
          filters.map(f => GmosImagingFilterInput(f, none)),
          GmosImagingInput.Create.Common(
            binning.some,
            ampReadMode.some,
            ampGain.some,
            roi.some
          )
        ).some,
        none,
        none,
        none,
        none,
        none,
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
        Flamingos2LongSlitInput.Create(disperser, filter, fpu, none, none, none, none, none, none).some,
        none,
        none,
        none,
        none,
        none,
        none,
        none,
        none,
        none,
        none,
        none,
        none,
        none,
        none
      )

  case class Flamingos2ImagingConfigs(
    filters: NonEmptyList[Flamingos2Filter]
  ) extends CalibrationConfigSubset derives Eq:
    def modeType: ObservingModeType = ObservingModeType.Flamingos2Imaging

  case class GnirsImagingConfigs(
    filters: NonEmptyList[GnirsFilter],
    camera:  GnirsCamera
  ) extends CalibrationConfigSubset derives Eq:
    def modeType: ObservingModeType = ObservingModeType.GnirsImaging

  /**
   * The IFU is calibrated through the IFU, not through the equivalent long slit,
   * so unlike MOS it does not reuse [[GmosNConfigs]] / [[GmosSConfigs]].
   */
  case class GmosNIfuConfigs(config: IfuConfig.GmosNorth) extends CalibrationConfigSubset derives Eq:
    def modeType: ObservingModeType = ObservingModeType.GmosNorthIfu

  case class GmosSIfuConfigs(config: IfuConfig.GmosSouth) extends CalibrationConfigSubset derives Eq:
    def modeType: ObservingModeType = ObservingModeType.GmosSouthIfu

  case object Igrins2Configs extends CalibrationConfigSubset derives Eq:
    def modeType: ObservingModeType = ObservingModeType.Igrins2LongSlit

  extension (mode: ObservingMode)
    def toConfigSubset: CalibrationConfigSubset =
      mode match
        case e: ExchangeConfig =>
          ExchangeConfigSubset(e)

        case f2: Flamingos2Config =>
          Flamingos2Configs(
            f2.disperser,
            f2.filter,
            f2.fpu
          )

        // MOS is calibrated as a long slit: the custom mask's slit width has a
        // 1:1 builtin long slit FPU, so a MOS and a long slit observation with
        // matching configuration share one calibration.
        case f2m: Flamingos2MosConfig =>
          Flamingos2Configs(
            f2m.disperser,
            f2m.filter,
            f2m.equivalentFpu
          )

        case f2i: Flamingos2ImagingConfig =>
          Flamingos2ImagingConfigs(f2i.filters.map(_.filter))

        case _: GhostConfig =>
          GhostConfigs

        case c: GnirsSpectroscopyConfig =>
          GnirsSpectroscopyConfigs(c)

        case gnm: GnirsImagingConfig =>
          GnirsImagingConfigs(gnm.filters.map(_.filter), gnm.camera)

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

        // MOS is calibrated as a long slit
        case gnm: MosConfig.GmosNorth =>
          GmosNConfigs(
            gnm.grating,
            gnm.filter,
            gnm.equivalentFpu,
            gnm.centralWavelength,
            gnm.xBin,
            gnm.yBin,
            gnm.ampReadMode,
            gnm.ampGain,
            gnm.roi
          )

        case gsm: MosConfig.GmosSouth =>
          GmosSConfigs(
            gsm.grating,
            gsm.filter,
            gsm.equivalentFpu,
            gsm.centralWavelength,
            gsm.xBin,
            gsm.yBin,
            gsm.ampReadMode,
            gsm.ampGain,
            gsm.roi
          )

        case gni: IfuConfig.GmosNorth =>
          GmosNIfuConfigs(gni)

        case gsi: IfuConfig.GmosSouth =>
          GmosSIfuConfigs(gsi)

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
        case _: Igrins2Config =>
          Igrins2Configs

        case v: VisitorConfig =>
          VisitorConfigSubset(v)
