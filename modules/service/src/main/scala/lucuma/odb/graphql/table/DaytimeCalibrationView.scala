// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import grackle.skunk.SkunkMapping
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GhostCodecs.*

trait DaytimeCalibrationView[F[_]] extends BaseMapping[F]:

  object DaytimeCalibrationView extends TableDef("v_daytime_calibration"):
    val ObservationId     = col("c_observation_id", observation_id)
    val ObservingModeType = col("c_observing_mode_type", daytime_calibration_observing_mode_type)
    val Instrument        = col("c_instrument", instrument)

    object Ghost:
      val Id             = col("c_ghost_id", observation_id.embedded)
      val ResolutionMode = col("c_ghost_resolution_mode", ghost_resolution_mode.embedded)
