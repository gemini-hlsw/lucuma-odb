// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.*
import skunk.codec.boolean.bool

trait SpectroscopyConfigOptionTable[F[_]] extends BaseMapping[F] {

  object SpectroscopyConfigOptionTable extends TableDef("t_spectroscopy_config_option") {
    val Instrument         = col("c_instrument",  instrument)
    val Index              = col("c_index",       int4_pos)

    val Name               = col("c_name",        text_nonempty)
    val FocalPlane         = col("c_focal_plane", focal_plane)

    val FpuLabel           = col("c_fpu_label",   text_nonempty)
    val SlitWidth          = col("c_slit_width",  angle_µas)
    val SlitLength         = col("c_slit_length", angle_µas)

    val DisperserLabel     = col("c_disperser_label", text_nonempty)
    val FilterLabel        = col("c_filter_label",    text_nonempty.opt)

    val WavelengthMin      = col("c_wavelength_min",      wavelength_pm)
    val WavelengthMax      = col("c_wavelength_max",      wavelength_pm)
    val WavelengthOptimal  = col("c_wavelength_optimal",  wavelength_pm)
    val WavelengthCoverage = col("c_wavelength_coverage", wavelength_pm)

    val Resolution         = col("c_resolution", int4_pos)
    val Ao                 = col("c_ao",         bool)
    val Capability         = col("c_capability", spectroscopy_capabilities.opt)
  }

}
