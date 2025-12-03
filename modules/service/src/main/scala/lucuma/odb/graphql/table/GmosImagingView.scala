// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GmosCodecs.*
import skunk.codec.numeric.int8

trait GmosImagingView[F[_]] extends BaseMapping[F]:

  class GmosImagingCommonColumns(implicit val tableName: TableName):

    val ObservationId: ColumnRef               = col("c_observation_id", observation_id)
    val Variant: ColumnRef                     = col("c_variant", gmos_imaging_variant)

    val ExplicitBin: ColumnRef                 = col("c_bin", gmos_binning.opt)
    val DefaultBin: ColumnRef                  = col("c_bin_default", gmos_binning)
    val ExplicitAmpReadMode: ColumnRef         = col("c_amp_read_mode", gmos_amp_read_mode.opt)
    val ExplicitAmpGain: ColumnRef             = col("c_amp_gain", gmos_amp_gain.opt)
    val ExplicitRoi: ColumnRef                 = col("c_roi", gmos_roi.opt)

    object Sky:
      val Count: ColumnRef = col("c_sky_count", int4_nonneg)
      val Seed: ColumnRef  = col("c_sky_seed",  int8)

    object Grouped:
      val ObservationId: ColumnRef   = col("c_grouped_observation_id", observation_id.embedded)
      val WavelengthOrder: ColumnRef = col("c_wavelength_order", wavelength_order)

    object Interleaved:
      val ObservationId: ColumnRef = col("c_interleaved_observation_id", observation_id.embedded)

    object PreImaging:
      val ObservationId: ColumnRef = col("c_pre_imaging_observation_id", observation_id.embedded)
      val Offset1P: ColumnRef      = col("c_pre_imaging_off1_p", angle_µas)
      val Offset1Q: ColumnRef      = col("c_pre_imaging_off1_q", angle_µas)
      val Offset2P: ColumnRef      = col("c_pre_imaging_off2_p", angle_µas)
      val Offset2Q: ColumnRef      = col("c_pre_imaging_off2_q", angle_µas)
      val Offset3P: ColumnRef      = col("c_pre_imaging_off3_p", angle_µas)
      val Offset3Q: ColumnRef      = col("c_pre_imaging_off3_q", angle_µas)
      val Offset4P: ColumnRef      = col("c_pre_imaging_off4_p", angle_µas)
      val Offset4Q: ColumnRef      = col("c_pre_imaging_off4_q", angle_µas)

  object GmosNorthImagingView extends TableDef("v_gmos_north_imaging"):

    val Filters: ColumnRef    = col("c_filters", _gmos_north_filter)
    val Common: GmosImagingCommonColumns = new GmosImagingCommonColumns()

  object GmosSouthImagingView extends TableDef("v_gmos_south_imaging"):

    val Filters: ColumnRef    = col("c_filters", _gmos_south_filter)
    val Common: GmosImagingCommonColumns = new GmosImagingCommonColumns()