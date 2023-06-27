// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.odb.graphql.table.ChronConditionsEntryView

import table.TargetView
import table.ProgramTable

trait ConditionsExpectationMapping[F[_]] extends ChronConditionsEntryView[F] {

  lazy val ConditionsExpectationMapping =
    ObjectMapping(
      tpe = ConditionsExpectationType,
      fieldMappings = List(
        SqlField("synthetic-id", ChronConditionsEntryView.Intuition.Expectation.SyntheticId, key = true, hidden = true),
        SqlField("type", ChronConditionsEntryView.Intuition.Expectation.Expectation),
        SqlObject("timeframe"),
      )
    )

}
