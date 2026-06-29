// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GnirsCodecs.*
import skunk.codec.numeric.int4

trait GnirsDynamicView[F[_]] extends BaseMapping[F]:

  object GnirsDynamicView extends TableDef("v_gnirs_dynamic"):
    val Id: ColumnRef                = col("c_step_id",            step_id)
    val Instrument: ColumnRef        = col("c_instrument",         instrument)
    val ExposureTime: ColumnRef      = col("c_exposure_time",      time_span)
    val Coadds: ColumnRef            = col("c_coadds",             int4_pos)
    val CentralWavelength: ColumnRef = col("c_central_wavelength", wavelength_pm)
    val Filter: ColumnRef            = col("c_filter",             gnirs_filter)
    val Decker: ColumnRef            = col("c_decker",             gnirs_decker)
    val FpuSlit: ColumnRef           = col("c_fpu_slit",           gnirs_fpu_slit.opt)
    val FpuOther: ColumnRef          = col("c_fpu_other",          gnirs_fpu_other.opt)
    val FpuIfu: ColumnRef            = col("c_fpu_ifu",            gnirs_fpu_ifu.opt)
    val Camera: ColumnRef            = col("c_camera",             gnirs_camera)
    val FocusMotorSteps: ColumnRef   = col("c_focus_motor_steps",  int4.opt)
    val ReadMode: ColumnRef          = col("c_read_mode",          gnirs_read_mode)

    object AcquisitionMirrorOut:
      val SyntheticId: ColumnRef = col("c_acquisition_mirror_out_id", step_id.embedded)
      val Prism: ColumnRef       = col("c_prism",                     gnirs_prism.embedded)
      val Grating: ColumnRef     = col("c_grating",                   gnirs_grating.embedded)
      val Wavelength: ColumnRef  = col("c_grating_wavelength",        wavelength_pm.embedded)
