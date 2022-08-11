// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package snippet
package mapping

import table.ObservationView
import edu.gemini.grackle.skunk.SkunkMapping

trait HourAngleRangeMapping[F[_]]
  extends ObservationView[F] { this: SkunkMapping[F] =>

  lazy val HourAngleRangeType = schema.ref("HourAngleRange")

  lazy val HourAngleRangeMapping =
    ObjectMapping(
      tpe = HourAngleRangeType,
      fieldMappings = List(
        SqlField("synthetic_id", ObservationView.ConstraintSet.ElevationRange.HourAngleRange.SyntheticId, key = true, hidden = true),
        SqlField("minHours", ObservationView.ConstraintSet.ElevationRange.HourAngleRange.HourAngleMin),
        SqlField("maxHours", ObservationView.ConstraintSet.ElevationRange.HourAngleRange.HourAngleMax)
      )
    )

}

