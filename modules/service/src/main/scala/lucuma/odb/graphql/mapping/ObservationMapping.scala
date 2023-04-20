// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import edu.gemini.grackle.TypeRef
import edu.gemini.grackle.skunk.SkunkMapping

import table.ObservationView
import table.ProgramTable
import lucuma.odb.graphql.table.TimingWindowTable

trait ObservationMapping[F[_]]
  extends ObservationView[F]
     with ProgramTable[F]  
     with TimingWindowTable[F] {

  lazy val ObservationMapping: ObjectMapping =
    ObjectMapping(
      tpe = ObservationType,
      fieldMappings = List(
        SqlField("id", ObservationView.Id, key = true),
        SqlField("programId", ObservationView.ProgramId, hidden = true),
        SqlField("existence", ObservationView.Existence, hidden = true),
        SqlField("title", ObservationView.Title),
        SqlField("subtitle", ObservationView.Subtitle),
        SqlField("status", ObservationView.Status),
        SqlField("activeStatus", ObservationView.ActiveStatus),
        SqlField("visualizationTime", ObservationView.VisualizationTime),
        SqlObject("posAngleConstraint"),
        SqlObject("targetEnvironment"),
        SqlObject("constraintSet"),
        SqlObject("timingWindows", Join(ObservationView.Id, TimingWindowTable.ObservationId)),
        SqlObject("scienceRequirements"),
        SqlObject("observingMode"),
        SqlObject("plannedTime"),
        SqlObject("program", Join(ObservationView.ProgramId, ProgramTable.Id))
      )
    )

}

