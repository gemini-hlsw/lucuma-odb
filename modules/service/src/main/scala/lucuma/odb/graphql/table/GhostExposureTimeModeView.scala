// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.exposure_time_mode_id
import lucuma.odb.util.Codecs.observation_id

trait GhostExposureTimeModeLinkView[F[_]] extends BaseMapping[F]:

  object GhostExposureTimeModeLinkView extends TableDef("v_ghost_exposure_time_mode_link"):
    val Id                = col("c_exposure_time_mode_id", exposure_time_mode_id)
    val BlueObservationId = col("c_blue_observation_id", observation_id.embedded)
    val RedObservationId  = col("c_red_observation_id",  observation_id.embedded)