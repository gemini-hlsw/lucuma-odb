// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.odb.graphql.table.ChronConditionsEntryView
import lucuma.odb.graphql.table.UserTable

import table.TargetView
import table.ProgramTable

trait ConditionsEntryMapping[F[_]] extends ChronConditionsEntryView[F] with UserTable[F]  {

  lazy val ConditionsEntryMapping =
    ObjectMapping(
      tpe = ConditionsEntryType,
      fieldMappings = List(
        SqlField("id", ChronConditionsEntryView.ChronId, key = true),
        SqlField("transactionId", ChronConditionsEntryView.TransationId),
        SqlField("timestamp", ChronConditionsEntryView.Timestamp),
        SqlObject("user", Join(ChronConditionsEntryView.UserId, UserTable.UserId)),
        SqlObject("measurement"),
        SqlObject("intuition"),
      )
    )

}

