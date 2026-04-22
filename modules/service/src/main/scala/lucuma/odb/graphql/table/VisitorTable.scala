// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import grackle.skunk.SkunkMapping
import lucuma.odb.util.Codecs.*

trait VisitorTable[F[_]] extends BaseMapping[F]:

  object VisitorTable extends TableDef("t_visitor"):
    val ObservationId     = col("c_observation_id", observation_id)
    val ObservingModeType = col("c_observing_mode_type", visitor_observing_mode_type)
    val CentralWavelength = col("c_central_wavelength", wavelength_pm)
    val GuideStarMinSep   = col("c_guide_star_min_sep", angle_µas)

