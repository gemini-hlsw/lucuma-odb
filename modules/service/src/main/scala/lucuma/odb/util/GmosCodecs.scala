// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.util

import lucuma.core.enums.GmosAmpCount
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosGratingOrder
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
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.model.sequence.gmos.StaticConfig
import skunk._
import skunk.codec.all._
import skunk.data.Type

trait GmosCodecs {

  import Codecs.enumerated

  val gmos_amp_count: Codec[GmosAmpCount] =
    enumerated(Type.varchar)

  val gmos_amp_gain: Codec[GmosAmpGain] =
    enumerated(Type.varchar)

  val gmos_amp_read_mode: Codec[GmosAmpReadMode] =
    enumerated(Type.varchar)

  val gmos_disperser_order: Codec[GmosGratingOrder] =
    enumerated(Type.varchar)

  val gmos_north_detector: Codec[GmosNorthDetector] =
    enumerated(Type.varchar)

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

  val gmos_south_filter: Codec[GmosSouthFilter] =
    enumerated(Type.varchar)

  val gmos_south_fpu: Codec[GmosSouthFpu] =
    enumerated(Type.varchar)

  val gmos_south_grating: Codec[GmosSouthGrating] =
    enumerated(Type.varchar)

  val gmos_south_stage_mode: Codec[GmosSouthStageMode] =
    enumerated(Type.varchar)

  val gmos_x_binning: Codec[GmosXBinning] =
    enumerated(Type.varchar)

  val gmos_y_binning: Codec[GmosYBinning] =
    enumerated(Type.varchar)

  val gmos_ccd_mode: Decoder[GmosCcdMode] =
    (
      gmos_x_binning *:
      gmos_y_binning *:
      gmos_amp_count *:
      gmos_amp_gain  *:
      gmos_amp_read_mode
    ).to[GmosCcdMode]

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