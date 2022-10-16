// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs._
import lucuma.odb.util.GmosCodecs._
import skunk.codec.all._

trait GmosNorthLongSlitTable[F[_]] extends BaseMapping[F] {

  object GmosNorthLongSlitTable extends TableDef("t_gmos_north_long_slit") {

    val ObservationId: ColumnRef            = col("c_observation_id", observation_id)

    val Grating: ColumnRef                  = col("c_grating", gmos_north_grating)
    val Filter: ColumnRef                   = col("c_filter", gmos_north_filter.opt)
    val Fpu: ColumnRef                      = col("c_fpu", gmos_north_fpu)
    val CentralWavelength: ColumnRef        = col("c_central_wavelength", wavelength_pm.opt)

    val XBin: ColumnRef                     = col("c_xbin", gmos_x_binning.opt)
    val YBin: ColumnRef                     = col("c_ybin", gmos_y_binning.opt)
    val AmpReadMode: ColumnRef              = col("c_amp_read_mode", gmos_amp_read_mode.opt)
    val AmpGain: ColumnRef                  = col("c_amp_gain", gmos_amp_gain.opt)
    val Roi: ColumnRef                      = col("c_roi", gmos_roi.opt)

    val WavelengthDithers: ColumnRef        = col("c_wavelength_dithers", text.opt)
    val SpatialOffsets: ColumnRef           = col("c_spatial_offsets", text.opt)

    val InitialGrating: ColumnRef           = col("c_initial_grating", gmos_north_grating)
    val InitialFilter: ColumnRef            = col("c_initial_filter", gmos_north_filter.opt)
    val InitialFpu: ColumnRef               = col("c_initial_fpu", gmos_north_fpu)
    val InitialCentralWavelength: ColumnRef = col("c_initial_central_wavelength", wavelength_pm.opt)

  }

}
