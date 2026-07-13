// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.*
import lucuma.odb.util.Flamingos2Codecs.*
import lucuma.odb.util.GmosCodecs.*
import lucuma.odb.util.GnirsCodecs.*
import skunk.codec.boolean.bool

trait ImagingConfigOptionTable[F[_]] extends BaseMapping[F] {

  object ImagingConfigOptionTable extends TableDef("t_imaging_config_option") {
    val Instrument         = col("c_instrument",   instrument)
    val Index              = col("c_index",        int4_pos)

    val Fov                = col("c_fov",          angle_µas)

    val FilterLabel        = col("c_filter_label", text_nonempty)

    val Ao                 = col("c_ao",           bool)

    val Capability         = col("c_capability",   imaging_capability.opt)

    val Site               = col("c_site",         site)
  }

  object ImagingConfigOptionGmosNorthTable extends TableDef("t_imaging_config_option_gmos_north") {
    val Instrument = col("c_instrument",  instrument)
    val Index      = col("c_index",       int4_pos)

    val Filter     = col("c_filter",      gmos_north_filter)
  }

  object ImagingConfigOptionGmosSouthTable extends TableDef("t_imaging_config_option_gmos_south") {
    val Instrument = col("c_instrument",  instrument)
    val Index      = col("c_index",       int4_pos)

    val Filter     = col("c_filter",      gmos_south_filter)
  }

  object ImagingConfigOptionFlamingos2Table extends TableDef("t_imaging_config_option_flamingos_2") {
    val Instrument = col("c_instrument",  instrument)
    val Index      = col("c_index",       int4_pos)

    val Filter     = col("c_filter",      flamingos_2_filter)
  }

  object ImagingConfigOptionGnirsTable extends TableDef("t_imaging_config_option_gnirs") {
    val Instrument = col("c_instrument",  instrument)
    val Index      = col("c_index",       int4_pos)

    val Filter     = col("c_filter",      gnirs_filter)
    val Camera     = col("c_camera",      gnirs_camera)
  }

}
