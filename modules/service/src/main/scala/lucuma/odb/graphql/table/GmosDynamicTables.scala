// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GmosCodecs.*
import org.tpolecat.typename.TypeName
import skunk.codec.text.varchar

trait GmosDynamicTables[F[_]] extends BaseMapping[F] {

  class GmosDynamicTable[G, L, U](
    name:    String,
    grating: skunk.Codec[G],
    filter:  skunk.Codec[L],
    fpu:     skunk.Codec[U]
  )(implicit gratingName: TypeName[G], filterName: TypeName[L], fpuName: TypeName[U]) extends TableDef(name) {
    val Id: ColumnRef            = col("c_step_id",           step_id)
    val Instrument: ColumnRef    = col("c_instrument",        instrument)

    val ExposureTime: ColumnRef  = col("c_exposure_time",     time_span)

    object CcdMode {
      val Xbin: ColumnRef        = col("c_xbin",              gmos_binning)
      val Ybin: ColumnRef        = col("c_ybin",              gmos_binning)
      val AmpCount: ColumnRef    = col("c_amp_count",         gmos_amp_count)
      val AmpGain: ColumnRef     = col("c_amp_gain",          gmos_amp_gain)
      val AmpReadMode: ColumnRef = col("c_amp_read_mode",     gmos_amp_read_mode)
    }

    val Dtax: ColumnRef          = col("c_dtax",              gmos_dtax)
    val Roi: ColumnRef           = col("c_roi",               gmos_roi)

    // Grating (if any)
    object Grating {
      val SyntheticId: ColumnRef = col("c_grating_id",         step_id.embedded)
      val Disperser: ColumnRef   = col("c_grating_disperser",  grating.embedded)
      val Order: ColumnRef       = col("c_grating_order",      gmos_grating_order.embedded)
      val Wavelength: ColumnRef  = col("c_grating_wavelength", wavelength_pm.embedded)
    }

    val Filter: ColumnRef       = col("c_filter",             filter.opt)

    // FPU (custom or builtin or none)
    object Fpu {
      val SyntheticId: ColumnRef   = col("c_fpu_id",                     step_id.embedded)
      object CustomMask {
        val SyntheticId: ColumnRef = col("c_fpu_custom_mask_id",         step_id.embedded)
        val Filename: ColumnRef    = col("c_fpu_custom_mask_filename",   varchar.embedded)
        val SlitWidth: ColumnRef   = col("c_fpu_custom_mask_slit_width", gmos_custom_slit_width.embedded)
      }
      val Builtin: ColumnRef       = col("c_fpu_builtin",                fpu.opt)
    }

    object CentralWavelength {
      val SyntheticId: ColumnRef = col("c_central_wavelength_id", step_id.embedded)
      val Value: ColumnRef       = col("c_central_wavelength",    wavelength_pm.embedded)
    }

  }

  object GmosNorthDynamicTable extends GmosDynamicTable(
    "v_gmos_north_dynamic",
    gmos_north_grating,
    gmos_north_filter,
    gmos_north_fpu
  )

  object GmosSouthDynamicTable extends GmosDynamicTable(
    "v_gmos_south_dynamic",
    gmos_south_grating,
    gmos_south_filter,
    gmos_south_fpu
  )

}
