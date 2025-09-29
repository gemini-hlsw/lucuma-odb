// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.*

trait ExposureTimeModeView[F[_]] extends BaseMapping[F]:

  object ExposureTimeModeView extends TableDef("v_exposure_time_mode"):
    val Id: ColumnRef = col("c_exposure_time_mode_id", exposure_time_mode_id)

    object SignalToNoise:
      val SyntheticId: ColumnRef = col("c_signal_to_noise_id", exposure_time_mode_id.embedded)
      val Value: ColumnRef       = col("c_signal_to_noise",    signal_to_noise.embedded)
      val At: ColumnRef          = col("c_signal_to_noise_at", wavelength_pm.embedded)

    object TimeAndCount:
      val SyntheticId: ColumnRef = col("c_time_and_count_id", exposure_time_mode_id.embedded)
      val Time: ColumnRef        = col("c_exposure_time",     time_span.embedded)
      val Count: ColumnRef       = col("c_exposure_count",    int4_pos.embedded)
      val At: ColumnRef          = col("c_signal_to_noise_at", wavelength_pm.embedded)

  object ExposureTimeModeLink extends TableDef("t_exposure_time_mode_link"):
    val ObservationId: ColumnRef      = col("c_observation_id",        observation_id)
    val ExposureTimeModeId: ColumnRef = col("c_exposure_time_mode_id", exposure_time_mode_id)