// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.*
import lucuma.odb.util.Flamingos2Codecs.*
import skunk.codec.text.varchar

trait Flamingos2DynamicView[F[_]] extends BaseMapping[F]:

  object Flamingos2DynamicView extends TableDef("v_flamingos_2_dynamic"):
    val Id: ColumnRef                = col("c_step_id",            step_id)
    val Instrument: ColumnRef        = col("c_instrument",         instrument)
    val ExposureTime: ColumnRef      = col("c_exposure_time",      time_span)
    val Disperser: ColumnRef         = col("c_disperser",          flamingos_2_disperser.opt)
    val Filter: ColumnRef            = col("c_filter",             flamingos_2_filter)
    val ReadMode: ColumnRef          = col("c_read_mode",          flamingos_2_read_mode)
    val LyotWheel: ColumnRef         = col("c_lyot_wheel",         flamingos_2_lyot_wheel)
    val Decker: ColumnRef            = col("c_decker",             flamingos_2_decker)
    val ReadoutMode: ColumnRef       = col("c_readout_mode",       flamingos_2_readout_mode)
    val Reads: ColumnRef             = col("c_reads",              flamingos_2_reads)
    val CentralWavelength: ColumnRef = col("c_central_wavelength", wavelength_pm)

    object Fpu:
      val SyntheticId: ColumnRef   = col("c_fpu_id",                     step_id.embedded)
      object CustomMask:
        val SyntheticId: ColumnRef = col("c_fpu_custom_mask_id",         step_id.embedded)
        val Filename: ColumnRef    = col("c_fpu_custom_mask_filename",   varchar.embedded)
        val SlitWidth: ColumnRef   = col("c_fpu_custom_mask_slit_width", flamingos_2_custom_slit_width.embedded)
      val Builtin: ColumnRef       = col("c_fpu_builtin",                flamingos_2_fpu.opt)