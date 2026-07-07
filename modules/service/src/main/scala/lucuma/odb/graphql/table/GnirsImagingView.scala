// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GnirsCodecs.*

trait GnirsImagingView[F[_]] extends BaseMapping[F]:

  object GnirsImagingFilterTable extends TableDef("t_gnirs_imaging_filter"):
    val ObservationId: ColumnRef = col("c_observation_id", observation_id)
    val Filter: ColumnRef        = col("c_filter", gnirs_filter)
    val Version: ColumnRef       = col("c_version", observing_mode_row_version)
    val ExposureTimeModeId       = col("c_exposure_time_mode_id", exposure_time_mode_id)
    val Role: ColumnRef          = col("c_role", exposure_time_mode_role)

  object GnirsImagingView extends TableDef("v_gnirs_imaging"):

    val ObservationId: ColumnRef    = col("c_observation_id", observation_id)
    val Variant: ColumnRef          = col("c_variant", imaging_variant)

    val Filters: ColumnRef          = col("c_filters", _gnirs_filter)

    val Camera: ColumnRef           = col("c_camera", gnirs_camera)
    val Coadds: ColumnRef           = col("c_coadds", int4_pos)

    val ReadMode: ColumnRef         = col("c_read_mode", gnirs_read_mode.opt)

    val WellDepth: ColumnRef        = col("c_well_depth", gnirs_well_depth.opt)
    val WellDepthDefault: ColumnRef = col("c_well_depth_default", gnirs_well_depth)

    object Sky:
      val Count: ColumnRef = col("c_sky_count", int4_nonneg)

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
