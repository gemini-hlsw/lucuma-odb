// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GmosCodecs.*
import skunk.codec.all.*

trait GmosIfuView[F[_]] extends BaseMapping[F]:

  class GmosIfuCommonColumns(implicit val tableName: TableName):

    val ObservationId: ColumnRef             = col("c_observation_id", observation_id)

    val CentralWavelength: ColumnRef         = col("c_central_wavelength", wavelength_pm)

    // At most one of the two is set; neither means the default sampling applies.
    val IfuAnalysisSumRadius: ColumnRef      = col("c_ifu_analysis_sum_radius", angle_µas.opt)
    val IfuAnalysisSingleOffset: ColumnRef   = col("c_ifu_analysis_single_offset", angle_µas.opt)

    val XBin: ColumnRef                      = col("c_xbin", gmos_binning.opt)
    val XBinDefault: ColumnRef               = col("c_xbin_default", gmos_binning)
    val YBin: ColumnRef                      = col("c_ybin", gmos_binning.opt)
    val YBinDefault: ColumnRef               = col("c_ybin_default", gmos_binning)
    val AmpReadMode: ColumnRef               = col("c_amp_read_mode", gmos_amp_read_mode.opt)
    val AmpGain: ColumnRef                   = col("c_amp_gain", gmos_amp_gain.opt)
    val Roi: ColumnRef                       = col("c_roi", gmos_roi.opt)

    val WavelengthDithers: ColumnRef         = col("c_wavelength_dithers", text.opt)
    val TelescopeConfigs: ColumnRef          = col("c_telescope_configs", text.opt)
    val TelescopeConfigsDefault: ColumnRef   = col("c_telescope_configs_default", text)
    val TelescopeConfigsEffective: ColumnRef = col("c_telescope_configs_effective", text)

    val InitialCentralWavelength: ColumnRef  = col("c_initial_central_wavelength", wavelength_pm)

  object GmosNorthIfuView extends TableDef("v_gmos_north_ifu"):

    val Grating: ColumnRef        = col("c_grating", gmos_north_grating)
    val Filter: ColumnRef         = col("c_filter", gmos_north_filter.opt)
    val Fpu: ColumnRef            = col("c_fpu", gmos_north_ifu_fpu)
    val AcquisitionFilter         = col("c_acquisition_filter", gmos_north_filter.opt)
    val AcquisitionFilterDefault  = col("c_acquisition_filter_default", gmos_north_filter)
    val AcquisitionRoi            = col("c_acquisition_roi", gmos_ifu_acquisition_roi.opt)
    val AcquisitionRoiDefault     = col("c_acquisition_roi_default", gmos_ifu_acquisition_roi)

    val InitialGrating: ColumnRef = col("c_initial_grating", gmos_north_grating)
    val InitialFilter: ColumnRef  = col("c_initial_filter", gmos_north_filter.opt)
    val InitialFpu: ColumnRef     = col("c_initial_fpu", gmos_north_ifu_fpu)

    val Common: GmosIfuCommonColumns = new GmosIfuCommonColumns()

  object GmosSouthIfuView extends TableDef("v_gmos_south_ifu"):

    val Grating: ColumnRef        = col("c_grating", gmos_south_grating)
    val Filter: ColumnRef         = col("c_filter", gmos_south_filter.opt)
    val Fpu: ColumnRef            = col("c_fpu", gmos_south_ifu_fpu)
    val AcquisitionFilter         = col("c_acquisition_filter", gmos_south_filter.opt)
    val AcquisitionFilterDefault  = col("c_acquisition_filter_default", gmos_south_filter)
    val AcquisitionRoi            = col("c_acquisition_roi", gmos_ifu_acquisition_roi.opt)
    val AcquisitionRoiDefault     = col("c_acquisition_roi_default", gmos_ifu_acquisition_roi)

    val InitialGrating: ColumnRef = col("c_initial_grating", gmos_south_grating)
    val InitialFilter: ColumnRef  = col("c_initial_filter", gmos_south_filter.opt)
    val InitialFpu: ColumnRef     = col("c_initial_fpu", gmos_south_ifu_fpu)

    val Common: GmosIfuCommonColumns = new GmosIfuCommonColumns()
