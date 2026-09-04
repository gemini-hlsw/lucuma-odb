// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.*
import skunk.codec.all.*

trait ExposureTimeModeView[F[_]] extends BaseMapping[F]:

  class BaseExposureTimeModeView(
    viewName: String
  ) extends TableDef(viewName):

    val Id: ColumnRef            = col("c_exposure_time_mode_id", exposure_time_mode_id)
    val ObservationId: ColumnRef = col("c_observation_id",        observation_id)
    val Role: ColumnRef          = col("c_role",                  exposure_time_mode_role)
    val IsExplicit: ColumnRef    = col("c_is_explicit",           bool)

    // Keys for mapping the same row as an *explicit* exposure time mode: null (and so a
    // null object, via `embedded`) unless the user actually set it.  Used for the GNIRS
    // acquisition's `explicitExposureTimeMode`, whose sibling `exposureTimeMode` always
    // resolves because it is the effective value.
    val ExplicitId: ColumnRef    = col("c_explicit_id",           exposure_time_mode_id.embedded)

    object SignalToNoise:
      val SyntheticId: ColumnRef         = col("c_signal_to_noise_id",          exposure_time_mode_id.embedded)
      val ExplicitSyntheticId: ColumnRef = col("c_explicit_signal_to_noise_id", exposure_time_mode_id.embedded)
      val Value: ColumnRef               = col("c_signal_to_noise",             signal_to_noise.embedded)
      val At: ColumnRef                  = col("c_signal_to_noise_at",          wavelength_pm.embedded)

    object TimeAndCount:
      val SyntheticId: ColumnRef         = col("c_time_and_count_id",          exposure_time_mode_id.embedded)
      val ExplicitSyntheticId: ColumnRef = col("c_explicit_time_and_count_id", exposure_time_mode_id.embedded)
      val Time: ColumnRef                = col("c_exposure_time",              time_span.embedded)
      val Count: ColumnRef               = col("c_exposure_count",             int4_pos.embedded)
      val At: ColumnRef                  = col("c_signal_to_noise_at",         wavelength_pm.embedded)

  object ExposureTimeModeView          extends BaseExposureTimeModeView("v_exposure_time_mode")
  object GhostBlueExposureTimeModeView extends BaseExposureTimeModeView("v_ghost_blue_exposure_time_mode")
  object GhostRedExposureTimeModeView  extends BaseExposureTimeModeView("v_ghost_red_exposure_time_mode")