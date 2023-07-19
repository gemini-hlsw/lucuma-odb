// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.odb.graphql.table.ChronConditionsEntryView

trait AddConditionsEntryResultMapping[F[_]] extends ChronConditionsEntryView[F] {

  lazy val AddConditionsEntryResultMapping: ObjectMapping =
    ObjectMapping(
      tpe = AddConditionsEntryResultType,
      fieldMappings = List(
        SqlField("id", ChronConditionsEntryView.ChronId, key = true, hidden = true),
        SqlObject("conditionsEntry"),
      )
    )

}

