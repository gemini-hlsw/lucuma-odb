// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import lucuma.odb.graphql.table.CallForProposalsView

trait CreateCallForProposalsResultMapping[F[_]] extends CallForProposalsView[F] {

  lazy val CreateCallForProposalsResultMapping =
    ObjectMapping(
      tpe = CreateCallForProposalsResultType,
      fieldMappings = List(
        SqlField("id", CallForProposalsView.Id, key = true, hidden = true),
        SqlObject("callForProposals")
      )
    )

}
