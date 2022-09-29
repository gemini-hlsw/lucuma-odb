// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import edu.gemini.grackle.skunk.SkunkMapping

import table.ObservationView

trait CoordinatesMapping[F[_]] extends ObservationView[F] {

  lazy val CoordinatesMapping =
    ObjectMapping(
      tpe = CoordinatesType,
      fieldMappings = List(
        SqlField("synthetic_id", ObservationView.TargetEnvironment.Coordinates.SyntheticId, key = true, hidden = true),
        SqlObject("ra"),
        SqlObject("dec")
      )
    )

}

