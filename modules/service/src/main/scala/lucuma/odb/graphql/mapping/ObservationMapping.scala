// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import edu.gemini.grackle.skunk.SkunkMapping

import table.ObservationView
import table.ProgramTable

trait ObservationMapping[F[_]]
  extends ObservationView[F]
     with ProgramTable[F] { this: SkunkMapping[F] =>

  lazy val ObservationType = schema.ref("Observation")

  lazy val ObservationMapping =
    ObjectMapping(
      tpe = ObservationType,
      fieldMappings = List(
        SqlField("id", ObservationView.Id, key = true),
        SqlField("programId", ObservationView.ProgramId, hidden=true),
        SqlField("existence", ObservationView.Existence, hidden = true),
        SqlField("subtitle", ObservationView.Subtitle),
        SqlField("status", ObservationView.Status),
        SqlField("activeStatus", ObservationView.ActiveStatus),
        SqlObject("targetEnvironment"),
        SqlObject("constraintSet"),
        SqlObject("program", Join(ObservationView.ProgramId, ProgramTable.Id))
      ),
    )

}

