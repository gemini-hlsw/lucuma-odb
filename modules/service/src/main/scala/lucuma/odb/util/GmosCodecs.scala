// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.util

import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosGratingOrder
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import skunk._
import skunk.codec.all._
import skunk.data.Type

trait GmosCodecs {

  import Codecs.enumerated

  val gmos_amp_gain: Codec[GmosAmpGain] =
    enumerated(Type.varchar)

  val gmos_amp_read_mode: Codec[GmosAmpReadMode] =
    enumerated(Type.varchar)

  val gmos_disperser_order: Codec[GmosGratingOrder] =
    enumerated(Type.varchar)

  val gmos_north_filter: Codec[GmosNorthFilter] =
    enumerated(Type.varchar)

  val gmos_north_fpu: Codec[GmosNorthFpu] =
    enumerated(Type.varchar)

  val gmos_north_grating: Codec[GmosNorthGrating] =
    enumerated(Type.varchar)

  val gmos_roi: Codec[GmosRoi] =
    enumerated(Type.varchar)

  val gmos_south_filter: Codec[GmosSouthFilter] =
    enumerated(Type.varchar)

  val gmos_south_fpu: Codec[GmosSouthFpu] =
    enumerated(Type.varchar)

  val gmos_south_grating: Codec[GmosSouthGrating] =
    enumerated(Type.varchar)

  val gmos_x_binning: Codec[GmosXBinning] =
    enumerated(Type.varchar)

  val gmos_y_binning: Codec[GmosYBinning] =
    enumerated(Type.varchar)

}

object GmosCodecs extends GmosCodecs