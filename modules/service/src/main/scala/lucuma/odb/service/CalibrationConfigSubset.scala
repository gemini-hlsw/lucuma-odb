// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsPrism
import lucuma.core.enums.ObservingModeType
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.gnirs.GnirsFpu
import lucuma.odb.graphql.input.Flamingos2LongSlitInput
import lucuma.odb.graphql.input.GmosIfuInput
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
import lucuma.odb.sequence.gmos.spectroscopy.Config as SpectroscopyConfig
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

  /**
   * GMOS spectroscopy calibrated per program and configuration. The subset is the creation input
   * itself, so matching and creation cannot disagree: a field can only tell two configurations
   * apart if it also reaches the calibration observation, and a calibration read back from the
   * database always matches the science it was created for.
   */
  sealed trait Gmos extends CalibrationConfigSubset:
    def centralWavelength: Wavelength
    def toInput:           ObservingModeInput.Create

    /** The ROI is the one field a calibration role may relax when matching. */
    def withRoi(roi: GmosRoi): Gmos

  case class GmosNConfigs(input: GmosLongSlitInput.Create.North) extends Gmos derives Eq:
    def modeType: ObservingModeType = ObservingModeType.GmosNorthLongSlit

    def centralWavelength: Wavelength =
      input.common.centralWavelength

    def toInput: ObservingModeInput.Create =
      ObservingModeInput.Create.Empty.copy(gmosNorthLongSlit = input.some)

    def withRoi(roi: GmosRoi): GmosNConfigs =
      GmosNConfigs(input.copy(common = input.common.copy(explicitRoi = roi.some)))

  case class GmosSConfigs(input: GmosLongSlitInput.Create.South) extends Gmos derives Eq:
    def modeType: ObservingModeType = ObservingModeType.GmosSouthLongSlit

    def centralWavelength: Wavelength =
      input.common.centralWavelength

    def toInput: ObservingModeInput.Create =
      ObservingModeInput.Create.Empty.copy(gmosSouthLongSlit = input.some)

    def withRoi(roi: GmosRoi): GmosSConfigs =
      GmosSConfigs(input.copy(common = input.common.copy(explicitRoi = roi.some)))

  /**
   * These modes are calibrated through the IFU, so the calibration repeats the science aperture
   * rather than falling back to an equivalent slit.
   */
  case class GmosNIfuConfigs(input: GmosIfuInput.Create.North) extends Gmos derives Eq:
    def modeType: ObservingModeType = ObservingModeType.GmosNorthIfu

    def centralWavelength: Wavelength =
      input.common.centralWavelength

    def toInput: ObservingModeInput.Create =
      ObservingModeInput.Create.Empty.copy(gmosNorthIfu = input.some)

    def withRoi(roi: GmosRoi): GmosNIfuConfigs =
      GmosNIfuConfigs(input.copy(common = input.common.copy(explicitRoi = roi.some)))

  case class GmosSIfuConfigs(input: GmosIfuInput.Create.South) extends Gmos derives Eq:
    def modeType: ObservingModeType = ObservingModeType.GmosSouthIfu

    def centralWavelength: Wavelength =
      input.common.centralWavelength

    def toInput: ObservingModeInput.Create =
      ObservingModeInput.Create.Empty.copy(gmosSouthIfu = input.some)

    def withRoi(roi: GmosRoi): GmosSIfuConfigs =
      GmosSIfuConfigs(input.copy(common = input.common.copy(explicitRoi = roi.some)))

  // Every effective value is pinned explicitly so the calibration reads back exactly as written,
  // which is what lets the input double as the matching key.
  private def longSlitCommon(c: SpectroscopyConfig[?, ?, ?]): GmosLongSlitInput.Create.Common =
    GmosLongSlitInput.Create.Common(
      centralWavelength        = c.centralWavelength,
      exposureTimeMode         = none,
      explicitXBin             = c.xBin.some,
      explicitYBin             = c.yBin.some,
      explicitAmpReadMode      = c.ampReadMode.some,
      explicitAmpGain          = c.ampGain.some,
      explicitRoi              = c.roi.some,
      explicitλDithers         = none,
      explicitTelescopeConfigs = none
    )

  private def ifuCommon(c: SpectroscopyConfig[?, ?, ?]): GmosIfuInput.Create.Common =
    GmosIfuInput.Create.Common(
      centralWavelength        = c.centralWavelength,
      exposureTimeMode         = none,
      explicitIfuAnalysis      = none,
      explicitXBin             = c.xBin.some,
      explicitYBin             = c.yBin.some,
      explicitAmpReadMode      = c.ampReadMode.some,
      explicitAmpGain          = c.ampGain.some,
      explicitRoi              = c.roi.some,
      explicitLambdaDithers    = none,
      explicitTelescopeConfigs = none
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
            GmosLongSlitInput.Create.North(
              grating     = gn.grating,
              filter      = gn.filter,
              fpu         = gn.fpu,
              common      = longSlitCommon(gn),
              acquisition = none
            )
          )

        case gs: Config.GmosSouth =>
          GmosSConfigs(
            GmosLongSlitInput.Create.South(
              grating     = gs.grating,
              filter      = gs.filter,
              fpu         = gs.fpu,
              common      = longSlitCommon(gs),
              acquisition = none
            )
          )

        // MOS is calibrated as a long slit
        case gnm: MosConfig.GmosNorth =>
          GmosNConfigs(
            GmosLongSlitInput.Create.North(
              grating     = gnm.grating,
              filter      = gnm.filter,
              fpu         = gnm.equivalentFpu,
              common      = longSlitCommon(gnm),
              acquisition = none
            )
          )

        case gsm: MosConfig.GmosSouth =>
          GmosSConfigs(
            GmosLongSlitInput.Create.South(
              grating     = gsm.grating,
              filter      = gsm.filter,
              fpu         = gsm.equivalentFpu,
              common      = longSlitCommon(gsm),
              acquisition = none
            )
          )

        case gni: IfuConfig.GmosNorth =>
          GmosNIfuConfigs(
            GmosIfuInput.Create.North(
              grating     = gni.grating,
              filter      = gni.filter,
              fpu         = gni.fpu,
              acquisition = none,
              common      = ifuCommon(gni)
            )
          )

        case gsi: IfuConfig.GmosSouth =>
          GmosSIfuConfigs(
            GmosIfuInput.Create.South(
              grating     = gsi.grating,
              filter      = gsi.filter,
              fpu         = gsi.fpu,
              acquisition = none,
              common      = ifuCommon(gsi)
            )
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
        case _: Igrins2Config =>
          Igrins2Configs

        case v: VisitorConfig =>
          VisitorConfigSubset(v)
