// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.*
import skunk.circe.codec.json.*
import skunk.codec.all.*

trait Igrins2LongSlitView[F[_]] extends BaseMapping[F]:

  object Igrins2LongSlitView extends TableDef("v_igrins_2_long_slit"):

    val ObservationId: ColumnRef         = col("c_observation_id", observation_id)

    val SaveSVCImages: ColumnRef         = col("c_save_svc_images", bool.opt)

    val SlitOffsetMode: ColumnRef          = col("c_slit_offset_mode", slit_offset_mode.opt)
    val SlitOffsetModeDefault: ColumnRef   = col("c_slit_offset_mode_default", slit_offset_mode.opt)
    val SlitOffsetModeEffective: ColumnRef = col("c_slit_offset_mode_effective", slit_offset_mode.opt)

    val TelescopeConfigs: ColumnRef          = col("c_telescope_configs", text.opt)
    val TelescopeConfigsDefault: ColumnRef   = col("c_telescope_configs_default", text)
    val TelescopeConfigsEffective: ColumnRef = col("c_telescope_configs_effective", text)

    val TelluricType: ColumnRef          = col("c_telluric_type", jsonb)
