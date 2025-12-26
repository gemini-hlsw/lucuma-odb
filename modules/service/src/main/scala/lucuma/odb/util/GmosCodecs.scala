// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.util

import cats.syntax.either.*
import cats.syntax.functor.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.GmosAmpCount
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosBinning
import lucuma.core.enums.GmosCustomSlitWidth
import lucuma.core.enums.GmosDtax
import lucuma.core.enums.GmosGratingOrder
import lucuma.core.enums.GmosImagingVariantType
import lucuma.core.enums.GmosLongSlitAcquisitionRoi
import lucuma.core.enums.GmosNorthDetector
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosNorthStageMode
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthDetector
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosSouthStageMode
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.core.model.sequence.gmos.GmosGratingConfig
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.core.util.Enumerated
import skunk.*
import skunk.codec.all.*
import skunk.data.Arr
import skunk.data.Type

trait GmosCodecs {

  import Codecs.enumerated
  import Codecs.time_span
  import Codecs.wavelength_pm

  val gmos_amp_count: Codec[GmosAmpCount] =
    enumerated(Type.varchar)

  val gmos_amp_gain: Codec[GmosAmpGain] =
    enumerated(Type.varchar)

  val gmos_amp_read_mode: Codec[GmosAmpReadMode] =
    enumerated(Type.varchar)

  val gmos_custom_slit_width: Codec[GmosCustomSlitWidth] =
    enumerated(Type.varchar)

  val gmos_dtax: Codec[GmosDtax] =
    enumerated(Type.varchar)

  val gmos_grating_order: Codec[GmosGratingOrder] =
    enumerated(Type.varchar)

  val gmos_imaging_variant: Codec[GmosImagingVariantType] =
    enumerated(Type("e_gmos_imaging_variant"))

  val gmos_long_slit_acquisition_roi: Codec[GmosLongSlitAcquisitionRoi] =
    enumerated(Type("e_gmos_long_slit_acquisition_roi"))

  val gmos_north_detector: Codec[GmosNorthDetector] =
    enumerated(Type.varchar)

  val _gmos_north_filter: Codec[Arr[GmosNorthFilter]] =
    Codec.array(_.tag, s => Enumerated[GmosNorthFilter].fromTag(s).toRight(s"Invalid GMOS-N filter tag: $s"), Type("_d_tag", List(Type("d_tag"))))

  val gmos_north_filter: Codec[GmosNorthFilter] =
    enumerated(Type.varchar)

  val gmos_north_fpu: Codec[GmosNorthFpu] =
    enumerated(Type.varchar)

  val gmos_north_grating: Codec[GmosNorthGrating] =
    enumerated(Type.varchar)

  val gmos_north_stage_mode: Codec[GmosNorthStageMode] =
    enumerated(Type.varchar)

  val gmos_roi: Codec[GmosRoi] =
    enumerated(Type.varchar)

  val gmos_south_detector: Codec[GmosSouthDetector] =
    enumerated(Type.varchar)

  val _gmos_south_filter: Codec[Arr[GmosSouthFilter]] =
    Codec.array(_.tag, s => Enumerated[GmosSouthFilter].fromTag(s).toRight(s"Invalid GMOS-S filter tag: $s"), Type("_d_tag", List(Type("d_tag"))))

  val gmos_south_filter: Codec[GmosSouthFilter] =
    enumerated(Type.varchar)

  val gmos_south_fpu: Codec[GmosSouthFpu] =
    enumerated(Type.varchar)

  val gmos_south_grating: Codec[GmosSouthGrating] =
    enumerated(Type.varchar)

  val gmos_south_stage_mode: Codec[GmosSouthStageMode] =
    enumerated(Type.varchar)

  val gmos_binning: Codec[GmosBinning] =
    enumerated(Type.varchar)

  val gmos_ccd_mode: Codec[GmosCcdMode] =
    (
      gmos_binning   *:
      gmos_binning   *:
      gmos_amp_count *:
      gmos_amp_gain  *:
      gmos_amp_read_mode
    ).imap { case (x, y, c, g, r) => GmosCcdMode(GmosXBinning(x), GmosYBinning(y), c, g, r) } { m =>
      (m.xBin.value, m.yBin.value, m.ampCount, m.ampGain, m.ampReadMode)
    }

  val gmos_fpu_mask_custom: Codec[GmosFpuMask.Custom] =
    (varchar *: gmos_custom_slit_width).eimap { case (n, w) =>
      NonEmptyString.from(n).leftMap(_ => "Custom mask filename cannot be empty").map { ne =>
        GmosFpuMask.Custom(ne, w)
      }
    } { c => (c.filename.value, c.slitWidth)}

  val gmos_north_fpu_mask: Codec[GmosFpuMask[GmosNorthFpu]] =
     (gmos_fpu_mask_custom.opt *: gmos_north_fpu.opt).eimap {
       case (None, None)       => Left("Neither custom nor builtin mask options are defined")
       case (Some(c), None)    => c.asRight.widen[GmosFpuMask[GmosNorthFpu]]
       case (None, Some(b))    => GmosFpuMask.Builtin(b).asRight.widen[GmosFpuMask[GmosNorthFpu]]
       case (Some(_), Some(_)) => Left("Both custom and builtin mask options are defined")
     } { m => (m.custom, m.builtinFpu) }

  val gmos_south_fpu_mask: Codec[GmosFpuMask[GmosSouthFpu]] =
     (gmos_fpu_mask_custom.opt *: gmos_south_fpu.opt).eimap {
       case (None, None)       => Left("Neither custom nor builtin mask options are defined")
       case (Some(c), None)    => c.asRight.widen[GmosFpuMask[GmosSouthFpu]]
       case (None, Some(b))    => GmosFpuMask.Builtin(b).asRight.widen[GmosFpuMask[GmosSouthFpu]]
       case (Some(_), Some(_)) => Left("Both custom and builtin mask options are defined")
     } { m => (m.custom, m.builtinFpu) }

  val gmos_north_grating_config: Codec[GmosGratingConfig.North] =
    (gmos_north_grating *: gmos_grating_order *: wavelength_pm).to[GmosGratingConfig.North]

  val gmos_south_grating_config: Codec[GmosGratingConfig.South] =
    (gmos_south_grating *: gmos_grating_order *: wavelength_pm).to[GmosGratingConfig.South]

  val gmos_north_dynamic: Codec[DynamicConfig.GmosNorth] =
    (time_span                     *:
     gmos_ccd_mode                 *:
     gmos_dtax                     *:
     gmos_roi                      *:
     gmos_north_grating_config.opt *:
     gmos_north_filter.opt         *:
     gmos_north_fpu_mask.opt
    ).to[DynamicConfig.GmosNorth]

  val gmos_south_dynamic: Codec[DynamicConfig.GmosSouth] =
    (time_span                     *:
     gmos_ccd_mode                 *:
     gmos_dtax                     *:
     gmos_roi                      *:
     gmos_south_grating_config.opt *:
     gmos_south_filter.opt         *:
     gmos_south_fpu_mask.opt
    ).to[DynamicConfig.GmosSouth]

  val gmos_north_static: Codec[StaticConfig.GmosNorth] =
    (
      gmos_north_detector    *:
      Codecs.mos_pre_imaging *:
      gmos_north_stage_mode
    ).imap { case (d, p, s) => StaticConfig.GmosNorth(s, d, p, None) } { g => (
      g.detector,
      g.mosPreImaging,
      g.stageMode
    )}

  val gmos_south_static: Codec[StaticConfig.GmosSouth] =
    (
      gmos_south_detector    *:
      Codecs.mos_pre_imaging *:
      gmos_south_stage_mode
    ).imap { case (d, p, s) => StaticConfig.GmosSouth(s, d, p, None) } { g => (
      g.detector,
      g.mosPreImaging,
      g.stageMode
    )}

}

object GmosCodecs extends GmosCodecs
