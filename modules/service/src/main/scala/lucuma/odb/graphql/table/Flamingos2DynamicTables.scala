// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.*
import lucuma.odb.util.Flamingos2Codecs.*
import org.tpolecat.typename.TypeName
import skunk.codec.numeric.int8

trait Flamingos2DynamicTables[F[_]] extends BaseMapping[F]:

  class Flamingos2DynamicTable[I](
    name:   String,
    idName: String,
    id:     skunk.Codec[I]
  )(using TypeName[I]) extends TableDef(name):
    val Id: ColumnRef                = col(idName,                id)
    val Instrument: ColumnRef        = col("c_instrument",         instrument)
    val ExposureTime: ColumnRef      = col("c_exposure_time",      time_span)
    val Disperser: ColumnRef         = col("c_disperser",          flamingos_2_disperser.opt)
    val Filter: ColumnRef            = col("c_filter",             flamingos_2_filter)
    val ReadMode: ColumnRef          = col("c_read_mode",          flamingos_2_read_mode)
    val LyotWheel: ColumnRef         = col("c_lyot_wheel",         flamingos_2_lyot_wheel)
    val Decker: ColumnRef            = col("c_decker",             flamingos_2_decker)
    val ReadoutMode: ColumnRef       = col("c_readout_mode",       flamingos_2_readout_mode)
    val Reads: ColumnRef             = col("c_reads",              flamingos_2_reads)
    val WindowCover: ColumnRef       = col("c_window_cover",       flamingos_2_window_cover.opt)
    val CentralWavelength: ColumnRef = col("c_central_wavelength", wavelength_pm)

    object Fpu:
      val SyntheticId: ColumnRef   = col("c_fpu_id",                     id.embedded)
      object CustomMask:
        val SyntheticId: ColumnRef  = col("c_fpu_custom_mask_id",            id.embedded)
        val AttachmentId: ColumnRef = col("c_fpu_custom_mask_attachment_id", attachment_id.opt)
        val SlitWidth: ColumnRef    = col("c_fpu_custom_mask_slit_width",    flamingos_2_custom_slit_width.embedded)
      val Builtin: ColumnRef       = col("c_fpu_builtin",                flamingos_2_fpu.opt)

  object Flamingos2DynamicView extends Flamingos2DynamicTable(
    "v_flamingos_2_dynamic",
    "c_step_id",
    step_id
  )

  object Flamingos2CalibrationSetupTable extends Flamingos2DynamicTable(
    "v_flamingos_2_daytime_calibration_setup",
    "c_set_id",
    int8
  )
