// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.*
import lucuma.odb.util.Igrins2Codecs.*
import skunk.codec.all.*

trait Igrins2LongSlitView[F[_]] extends BaseMapping[F]:

  object Igrins2LongSlitView extends TableDef("v_igrins_2_long_slit"):

    val ObservationId: ColumnRef         = col("c_observation_id", observation_id)

    val OffsetMode: ColumnRef            = col("c_offset_mode", igrins_2_offset_mode.opt)

    val SaveSVCImages: ColumnRef         = col("c_save_svc_images", bool.opt)

    val Offsets: ColumnRef               = col("c_spatial_offsets", text.opt)

    val DefaultOffsets: ColumnRef        = col("c_default_spatial_offsets", text)
