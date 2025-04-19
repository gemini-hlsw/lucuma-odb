// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping
import lucuma.odb.graphql.table.ChronConditionsEntryView

trait ConditionsIntuitionMapping[F[_]] extends ChronConditionsEntryView[F] {

  lazy val ConditionsIntuitionMapping =
    ObjectMapping(ConditionsIntuitionType)(
      SqlField("synthetic-id", ChronConditionsEntryView.Intuition.SyntheticId, key = true, hidden = true),
      SqlField("seeingTrend", ChronConditionsEntryView.Intuition.SeeingTrend),
      SqlObject("expectation"),
    )

}

