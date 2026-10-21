// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.*
import skunk.codec.boolean.bool
import skunk.codec.numeric.int4
import skunk.codec.numeric.int8

trait DaytimeCalibrationSetView[F[_]] extends BaseMapping[F]:

  object DaytimeCalibrationSetView extends TableDef("v_daytime_calibration_set"):
    val Id: ColumnRef            = col("c_set_id",         int8)
    val ObservationId: ColumnRef = col("c_observation_id", observation_id)
    val Position: ColumnRef      = col("c_position",       int4)
    val Kind: ColumnRef          = col("c_kind",           calibration_set_type)
    val Count: ColumnRef         = col("c_count",          int4_pos)

    object Gcal:
      val SyntheticId: ColumnRef = col("c_gcal_id",        int8.embedded)
      val Continuum: ColumnRef   = col("c_gcal_continuum", gcal_continuum.opt)
      val ArArc: ColumnRef       = col("c_gcal_ar_arc",    bool)
      val CuarArc: ColumnRef     = col("c_gcal_cuar_arc",  bool)
      val TharArc: ColumnRef     = col("c_gcal_thar_arc",  bool)
      val XeArc: ColumnRef       = col("c_gcal_xe_arc",    bool)
      val Filter: ColumnRef      = col("c_gcal_filter",    gcal_filter.embedded)
      val Diffuser: ColumnRef    = col("c_gcal_diffuser",  gcal_diffuser.embedded)
      val Shutter: ColumnRef     = col("c_gcal_shutter",   gcal_shutter.embedded)
