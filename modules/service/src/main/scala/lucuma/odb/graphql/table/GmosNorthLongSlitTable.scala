// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs._
import lucuma.odb.util.GmosCodecs._
import skunk.codec.all._

/*
  c_observation_id     d_observation_id NOT NULL PRIMARY KEY REFERENCES t_observation(c_observation_id),

  c_grating            d_tag            NOT NULL             REFERENCES t_gmos_north_disperser(c_tag),
  c_filter             d_tag            NULL DEFAULT NULL    REFERENCES t_gmos_north_filter(c_tag),
  c_fpu                d_tag            NOT NULL             REFERENCES t_gmos_north_fpu(c_tag),

  c_wavelength         d_wavelength_pm  NULL DEFAULT NULL,
  c_xbin               d_tag            NULL DEFAULT NULL   REFERENCES t_gmos_binning(c_tag),
  c_ybin               d_tag            NULL DEFAULT NULL   REFERENCES t_gmos_binning(c_tag),
  c_amp_read_mode      d_tag            NULL DEFAULT NULL   REFERENCES t_gmos_amp_read_mode(c_tag),
  c_amp_gain           d_tag            NULL DEFAULT NULL   REFERENCES t_gmos_amp_gain(c_tag),
  c_amp_roi            d_tag            NULL DEFAULT NULL   REFERENCES t_gmos_roi(c_tag),

  -- stuff wavelength dithers and offsets into a string until grackle supports array columns?
  c_wavelength_dithers text             NULL DEFAULT NULL,
  c_spatial_offsets    text             NULL DEFAULT NULL,

  CONSTRAINT wavelength_dither_format CHECK (c_wavelength_dithers ~ '^-?\d+(?:,-?\d+)*$'),
  CONSTRAINT offset_format            CHECK (c_spatial_offsets ~ '^\(-?\d+,-?\d+\)(?:,\(-?\d+,-?\d+\))*$')
*/

trait GmosNorthLongSlitTable[F[_]] extends BaseMapping[F] {

  object GmosNorthLongSlitTable extends TableDef("t_obs_mode_gmos_north_long_slit") {

    val ObservationId: ColumnRef     = col("c_observation_id", observation_id)

    val Grating: ColumnRef           = col("c_grating", gmos_north_grating)
    val Filter: ColumnRef            = col("c_filter", gmos_north_filter.opt)
    val Fpu: ColumnRef               = col("c_fpu", gmos_north_fpu)
    val CentralWavelength: ColumnRef = col("c_central_wavelength", wavelength_pm.opt)

    val XBin: ColumnRef              = col("c_xbin", gmos_x_binning.opt)
    val YBin: ColumnRef              = col("c_ybin", gmos_y_binning.opt)
    val AmpReadMode: ColumnRef       = col("c_amp_read_mode", gmos_amp_read_mode.opt)
    val AmpGain: ColumnRef           = col("c_amp_gain", gmos_amp_gain.opt)
    val Roi: ColumnRef               = col("c_roi", gmos_roi.opt)

    val WavelengthDithers: ColumnRef = col("c_wavelength_dithers", text.opt)
    val SpatialOffsets: ColumnRef    = col("c_spatial_offsets", text.opt)

    val InitialGrating: ColumnRef           = col("c_initial_grating", gmos_north_grating)
    val InitialFilter: ColumnRef            = col("c_initial_filter", gmos_north_filter.opt)
    val InitialFpu: ColumnRef               = col("c_initial_fpu", gmos_north_fpu)
    val InitialCentralWavelength: ColumnRef = col("c_initial_central_wavelength", wavelength_pm.opt)

  }

}
