// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import edu.gemini.grackle.skunk.SkunkMapping

import table.ObservationView

trait ElevationRangeMapping[F[_]]
  extends ObservationView[F] { this: SkunkMapping[F] =>

  lazy val ElevationRangeType = schema.ref("ElevationRange")

  lazy val ElevationRangeMapping =
    ObjectMapping(
      tpe = ElevationRangeType,
      fieldMappings = List(
        SqlField("id", ObservationView.Id, key = true, hidden = true),
        SqlObject("airMass"),
        SqlObject("hourAngle")
      )
    )

}

