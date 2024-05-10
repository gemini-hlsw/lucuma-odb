// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping

import table.ObservationView

trait HourAngleRangeMapping[F[_]] extends ObservationView[F] {

  lazy val HourAngleRangeMapping =
    ObjectMapping(HourAngleRangeType)(
      SqlField("synthetic_id", ObservationView.ConstraintSet.ElevationRange.HourAngleRange.SyntheticId, key = true, hidden = true),
      SqlField("minHours", ObservationView.ConstraintSet.ElevationRange.HourAngleRange.HourAngleMin),
      SqlField("maxHours", ObservationView.ConstraintSet.ElevationRange.HourAngleRange.HourAngleMax)
    )

}

