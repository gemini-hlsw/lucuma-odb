// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import edu.gemini.grackle.skunk.SkunkMapping

import table.ObservationView

trait AirMassRangeMapping[F[_]] extends ObservationView[F] {

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

