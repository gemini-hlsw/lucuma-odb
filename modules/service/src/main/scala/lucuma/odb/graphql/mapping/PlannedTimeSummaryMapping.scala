// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import cats.syntax.all._
import edu.gemini.grackle.Mapping
import edu.gemini.grackle.Path
import edu.gemini.grackle.Predicate._
import edu.gemini.grackle.Query
import edu.gemini.grackle.Query._
import edu.gemini.grackle.Result
import edu.gemini.grackle.TypeRef
import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.data.Existence

import binding._
import input._
import table._

trait PlannedTimeSummaryMapping[F[_]]
  extends ProgramTable[F] with ObservationView[F] {

  lazy val PlannedTimeSummaryMapping: TypeMapping =
    SwitchMapping(PlannedTimeSummaryType, List(
      ProgramType / "plannedTime"     -> plannedTimeSummaryMapping(ProgramTable.Id),
      ObservationType / "plannedTime" -> plannedTimeSummaryMapping(ObservationView.Id),
    ))

  private def plannedTimeSummaryMapping(keyColumnRef: ColumnRef): ObjectMapping =
    ObjectMapping(
      tpe = PlannedTimeSummaryType,
      fieldMappings = List(
        SqlField("id", keyColumnRef, key = true, hidden = true),
        SqlObject("pi"),
        SqlObject("execution"),
        SqlObject("uncharged"),
      )
    )

}

