// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GmosCodecs.*

trait SpectroscopyConfigOptionGmosNorthTable[F[_]] extends BaseMapping[F] {

  object SpectrsocopyConfigOptionGmosNorthTable extends TableDef("t_spectroscopy_config_option_gmos_north") {
    val Instrument = col("c_instrument",  instrument)
    val Index      = col("c_index",       int4_pos)

    val Fpu        = col("c_fpu",       gmos_north_fpu)
    val Disperser  = col("c_disperser", gmos_north_grating)
    val Filter     = col("c_filter",    gmos_north_filter.opt)
  }

}
