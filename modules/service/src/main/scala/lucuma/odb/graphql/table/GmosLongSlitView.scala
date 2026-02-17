// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GmosCodecs.*
import skunk.circe.codec.json.*
import skunk.codec.all.*

trait GmosLongSlitView[F[_]] extends BaseMapping[F]:

  class GmosLongSlitCommonColumns(implicit val tableName: TableName):

    val ObservationId: ColumnRef            = col("c_observation_id", observation_id)

    val CentralWavelength: ColumnRef        = col("c_central_wavelength", wavelength_pm)

    val XBin: ColumnRef                     = col("c_xbin", gmos_binning.opt)
    val XBinDefault: ColumnRef              = col("c_xbin_default", gmos_binning)
    val YBin: ColumnRef                     = col("c_ybin", gmos_binning.opt)
    val YBinDefault: ColumnRef              = col("c_ybin_default", gmos_binning)
    val AmpReadMode: ColumnRef              = col("c_amp_read_mode", gmos_amp_read_mode.opt)
    val AmpGain: ColumnRef                  = col("c_amp_gain", gmos_amp_gain.opt)
    val Roi: ColumnRef                      = col("c_roi", gmos_roi.opt)
    val AcquisitionRoi: ColumnRef           = col("c_acquisition_roi", gmos_long_slit_acquisition_roi.opt)
    val AcquisitionRoiDefault: ColumnRef    = col("c_acquisition_roi_default", gmos_long_slit_acquisition_roi)

    val WavelengthDithers: ColumnRef        = col("c_wavelength_dithers", text.opt)
    val Offsets: ColumnRef                  = col("c_offsets", text.opt)

    val InitialCentralWavelength: ColumnRef = col("c_initial_central_wavelength", wavelength_pm)

    val ImageQuality: ColumnRef             = col("c_image_quality", image_quality_preset)
    val SourceProfile: ColumnRef            = col("c_source_profile", jsonb.opt)

  object GmosNorthLongSlitView extends TableDef("v_gmos_north_long_slit"):

    val Grating: ColumnRef        = col("c_grating", gmos_north_grating)
    val Filter: ColumnRef         = col("c_filter", gmos_north_filter.opt)
    val AcquisitionFilter         = col("c_acquisition_filter", gmos_north_filter.opt)
    val AcquisitionFilterDefault  = col("c_acquisition_filter_default", gmos_north_filter)
    val Fpu: ColumnRef            = col("c_fpu", gmos_north_fpu)

    val InitialGrating: ColumnRef = col("c_initial_grating", gmos_north_grating)
    val InitialFilter: ColumnRef  = col("c_initial_filter", gmos_north_filter.opt)
    val InitialFpu: ColumnRef     = col("c_initial_fpu", gmos_north_fpu)

    val Common: GmosLongSlitCommonColumns     = new GmosLongSlitCommonColumns()

  object GmosSouthLongSlitView extends TableDef("v_gmos_south_long_slit"):

    val Grating: ColumnRef        = col("c_grating", gmos_south_grating)
    val Filter: ColumnRef         = col("c_filter", gmos_south_filter.opt)
    val AcquisitionFilter         = col("c_acquisition_filter", gmos_south_filter.opt)
    val AcquisitionFilterDefault  = col("c_acquisition_filter_default", gmos_south_filter)
    val Fpu: ColumnRef            = col("c_fpu", gmos_south_fpu)

    val InitialGrating: ColumnRef = col("c_initial_grating", gmos_south_grating)
    val InitialFilter: ColumnRef  = col("c_initial_filter", gmos_south_filter.opt)
    val InitialFpu: ColumnRef     = col("c_initial_fpu", gmos_south_fpu)

    val Common: GmosLongSlitCommonColumns     = new GmosLongSlitCommonColumns()