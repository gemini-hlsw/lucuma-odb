// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GnirsCodecs.*
import skunk.codec.all.*

trait GnirsLongSlitView[F[_]] extends BaseMapping[F]:

  object GnirsLongSlitView extends TableDef("v_gnirs_long_slit"):

    val ObservationId: ColumnRef    = col("c_observation_id", observation_id)

    val Grating: ColumnRef           = col("c_grating", gnirs_grating.opt)
    val Prism: ColumnRef             = col("c_prism", gnirs_prism.opt)
    val GratingWavelength: ColumnRef = col("c_grating_wavelength", wavelength_pm.opt)
    val GratingWavelengthValue: ColumnRef = col("c_grating_wavelength", wavelength_pm)

    // Initial acquisition mirror (always set, always Out)
    val InitialGrating: ColumnRef   = col("c_initial_grating", gnirs_grating)
    val InitialPrism: ColumnRef     = col("c_initial_prism", gnirs_prism)

    // Camera
    val Camera: ColumnRef           = col("c_camera", gnirs_camera)
    val InitialCamera: ColumnRef    = col("c_initial_camera", gnirs_camera)

    // FPU
    val Fpu: ColumnRef              = col("c_fpu", gnirs_fpu_slit)
    val InitialFpu: ColumnRef       = col("c_initial_fpu", gnirs_fpu_slit)

    // Filter
    val Filter: ColumnRef           = col("c_filter", gnirs_filter)
    val InitialFilter: ColumnRef    = col("c_initial_filter", gnirs_filter)

    // Coadds
    val Coadds: ColumnRef           = col("c_coadds", int4_pos)

    // Explicit overrides (nullable)
    val ExplicitDecker: ColumnRef   = col("c_decker", gnirs_decker.opt)
    val FocusMotorSteps: ColumnRef  = col("c_focus_motor_steps", int4.opt)
    val ExplicitReadMode: ColumnRef = col("c_read_mode", gnirs_obs_read_mode.opt)
    val ExplicitWellDepth: ColumnRef = col("c_well_depth", gnirs_well_depth.opt)

    // Slit telescope configs: explicit overrides (both nullable)
    val ExplicitSlitOffsetMode: ColumnRef   = col("c_slit_offset_mode", slit_offset_mode.opt)
    val ExplicitTelescopeConfigs: ColumnRef = col("c_telescope_configs", text.opt)

    // View-computed defaults/effective for telescope configs
    val DefaultSlitOffsetMode: ColumnRef     = col("c_slit_offset_mode_default", slit_offset_mode)
    val DefaultTelescopeConfigs: ColumnRef   = col("c_telescope_configs_default", text)
    val SlitOffsetModeEffective: ColumnRef   = col("c_slit_offset_mode_effective", slit_offset_mode)
    val TelescopeConfigsEffective: ColumnRef = col("c_telescope_configs_effective", text)

    // Acquisition config (ETM stored in t_exposure_time_mode via FK)
    val AcqType: ColumnRef          = col("c_acq_type", gnirs_acquisition_type)
    val AcqCoadds: ColumnRef        = col("c_acq_coadds", int4_pos)
    val AcqFilter: ColumnRef        = col("c_acq_filter", gnirs_filter)
    val AcqOffsetP: ColumnRef       = col("c_acq_offset_p", angle_µas.opt)
    val AcqOffsetQ: ColumnRef       = col("c_acq_offset_q", angle_µas.opt)

    // View-computed defaults
    val DefaultDecker: ColumnRef    = col("c_decker_default", gnirs_decker)
    val DefaultGratingWavelength: ColumnRef    = col("c_grating_wavelength_default", wavelength_pm)
    val GratingWavelengthEffective: ColumnRef  = col("c_grating_wavelength_effective", wavelength_pm)
    val DefaultReadMode: ColumnRef  = col("c_read_mode_default", gnirs_obs_read_mode)
    val DefaultWellDepth: ColumnRef = col("c_well_depth_default", gnirs_well_depth)

    // Effective grating/prism: COALESCE(explicit, initial)
    val GratingEffective: ColumnRef = col("c_grating_effective", gnirs_grating)
    val PrismEffective: ColumnRef   = col("c_prism_effective", gnirs_prism)
