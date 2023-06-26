// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.odb.graphql.table.ChronConditionsEntryView

import table.TargetView
import table.ProgramTable

trait ConditionsIntuitionMapping[F[_]] extends ChronConditionsEntryView[F] {

  lazy val ConditionsIntuitionMapping =
    ObjectMapping(
      tpe = ConditionsIntuitionType,
      fieldMappings = List(
        SqlField("synthetic-id", ChronConditionsEntryView.Intuition.SyntheticId, key = true, hidden = true),
        SqlField("seeingTrend", ChronConditionsEntryView.Intuition.SeeingTrend),
        SqlObject("expectation"),
      )
    )

}

