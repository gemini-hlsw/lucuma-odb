// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package snippet
package mapping

import table.ObservationView

import edu.gemini.grackle.skunk.SkunkMapping

trait AirMassRangeMapping[F[_]]
  extends ObservationView[F] { this: SkunkMapping[F] =>

  lazy val AirMassRangeType = schema.ref("AirMassRange")

  lazy val AirMassRangeMapping =
    ObjectMapping(
      tpe = AirMassRangeType,
      fieldMappings = List(
        SqlField("synthetic_id", ObservationView.ConstraintSet.ElevationRange.AirMassRange.SyntheticId, key = true, hidden = true),
        SqlField("min", ObservationView.ConstraintSet.ElevationRange.AirMassRange.AirMassMin),
        SqlField("max", ObservationView.ConstraintSet.ElevationRange.AirMassRange.AirMassMax)
      )
    )

}

