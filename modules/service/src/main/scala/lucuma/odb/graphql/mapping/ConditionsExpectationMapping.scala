// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping
import lucuma.odb.graphql.table.ChronConditionsEntryView

trait ConditionsExpectationMapping[F[_]] extends ChronConditionsEntryView[F] {

  lazy val ConditionsExpectationMapping =
    ObjectMapping(ConditionsExpectationType)(
      SqlField("synthetic-id", ChronConditionsEntryView.Intuition.Expectation.SyntheticId, key = true, hidden = true),
      SqlField("type", ChronConditionsEntryView.Intuition.Expectation.Expectation),
      SqlObject("timeframe"),
    )

}

