// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GnirsCodecs.*
import skunk.circe.codec.json.*
import skunk.codec.all.*

trait GnirsSpectroscopyView[F[_]] extends BaseMapping[F]:

  object GnirsSpectroscopyView extends TableDef("v_gnirs_spectroscopy"):

    val ObservationId: ColumnRef    = col("c_observation_id", observation_id)

    val Grating: ColumnRef           = col("c_grating", gnirs_grating.opt)
    val Prism: ColumnRef             = col("c_prism", gnirs_prism.opt)
    // Central wavelength override (nullable); effective falls back to the initial value.
    val CentralWavelength: ColumnRef = col("c_central_wavelength", wavelength_pm.opt)

    // Initial acquisition mirror (always set, always Out)
    val InitialGrating: ColumnRef           = col("c_initial_grating", gnirs_grating)
    val InitialPrism: ColumnRef             = col("c_initial_prism", gnirs_prism)
    val InitialCentralWavelength: ColumnRef = col("c_initial_central_wavelength", wavelength_pm)

    // Camera
    val Camera: ColumnRef           = col("c_camera", gnirs_camera)
    val InitialCamera: ColumnRef    = col("c_initial_camera", gnirs_camera)

    // FPU: exactly one of slit / ifu is non-null per row.
    val FpuSlit: ColumnRef          = col("c_fpu_slit", gnirs_fpu_slit.opt)
    val FpuIfu: ColumnRef           = col("c_fpu_ifu", gnirs_fpu_ifu.opt)
    // Embedded (FailedJoin-on-null) aliases used as discriminator keys in the
    // configuration mappings: c_fpu_slit is set only for long slit rows and
    // c_fpu_ifu only for IFU rows, so keying on them yields a null object for
    // the other variant instead of reading a null required field.
    val FpuSlitConfig: ColumnRef    = col("c_fpu_slit", gnirs_fpu_slit.embedded)
    val FpuIfuConfig: ColumnRef     = col("c_fpu_ifu", gnirs_fpu_ifu.embedded)
    val InitialFpuSlit: ColumnRef   = col("c_initial_fpu_slit", gnirs_fpu_slit.opt)
    val InitialFpuIfu: ColumnRef    = col("c_initial_fpu_ifu", gnirs_fpu_ifu.opt)

    // Filter
    val Filter: ColumnRef           = col("c_filter", gnirs_filter)
    val InitialFilter: ColumnRef    = col("c_initial_filter", gnirs_filter)

    // Coadds
    val Coadds: ColumnRef           = col("c_coadds", int4_pos)

    // Explicit overrides (nullable)
    val ExplicitDecker: ColumnRef   = col("c_decker", gnirs_decker.opt)
    val FocusMotorSteps: ColumnRef  = col("c_focus_motor_steps", int4.opt)
    val ExplicitReadMode: ColumnRef = col("c_read_mode", gnirs_read_mode.opt)
    val ExplicitWellDepth: ColumnRef = col("c_well_depth", gnirs_well_depth.opt)

    // Slit telescope configs: explicit overrides (both nullable)
    val ExplicitSlitOffsetMode: ColumnRef   = col("c_slit_offset_mode", slit_offset_mode.opt)
    val ExplicitTelescopeConfigs: ColumnRef = col("c_telescope_configs", text.opt)

    // View-computed defaults/effective for telescope configs
    // Slit offset mode default/effective are NULL for IFU rows.
    val DefaultSlitOffsetMode: ColumnRef     = col("c_slit_offset_mode_default", slit_offset_mode.opt)
    val DefaultTelescopeConfigs: ColumnRef   = col("c_telescope_configs_default", text)
    val SlitOffsetModeEffective: ColumnRef   = col("c_slit_offset_mode_effective", slit_offset_mode.opt)
    val TelescopeConfigsEffective: ColumnRef = col("c_telescope_configs_effective", text)

    // Acquisition config (ETM stored in t_exposure_time_mode via FK)
    val AcqType: ColumnRef          = col("c_acq_type", gnirs_acquisition_type.opt)
    val AcqCoadds: ColumnRef        = col("c_acq_coadds", int4_pos)
    val AcqFilter: ColumnRef        = col("c_acq_filter", gnirs_filter.opt)
    val AcqSkyOffsetP: ColumnRef    = col("c_acq_sky_offset_p", angle_µas.opt)
    val AcqSkyOffsetQ: ColumnRef    = col("c_acq_sky_offset_q", angle_µas.opt)

    // View-computed defaults and effective values
    val DefaultDecker: ColumnRef           = col("c_decker_default", gnirs_decker)
    val DeckerEffective: ColumnRef         = col("c_decker_effective", gnirs_decker)
    val DefaultWellDepth: ColumnRef        = col("c_well_depth_default", gnirs_well_depth)
    val WellDepthEffective: ColumnRef      = col("c_well_depth_effective", gnirs_well_depth)

    // Effective grating/prism/central wavelength: COALESCE(explicit, initial)
    val GratingEffective: ColumnRef           = col("c_grating_effective", gnirs_grating)
    val PrismEffective: ColumnRef             = col("c_prism_effective", gnirs_prism)
    val CentralWavelengthEffective: ColumnRef = col("c_central_wavelength_effective", wavelength_pm)

    // Telluric type (stored as jsonb)
    val TelluricType: ColumnRef     = col("c_telluric_type", jsonb)
