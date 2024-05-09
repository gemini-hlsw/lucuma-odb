// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping
import lucuma.odb.graphql.table.ProposalStatusTable

trait ProposalStatusMetaMapping[F[_]] extends ProposalStatusTable[F] {
  
  lazy val ProposalStatusMetaMapping = 
    ObjectMapping(ProposalStatusMetaType)(
      SqlField("tag", ProposalStatusTable.Tag, key = true),
      SqlField("name", ProposalStatusTable.Name),
      SqlField("ordinal", ProposalStatusTable.Ordinal, hidden = true)
    )

}
