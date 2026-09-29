// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import lucuma.odb.graphql.table.ProposalStatusChangeView

trait ProposalStatusChangeMapping[F[_]] extends ProposalStatusChangeView[F]:

  lazy val ProposalStatusChangeMapping: ObjectMapping =
    ObjectMapping(ProposalStatusChangeType)(
      SqlField("id",        ProposalStatusChangeView.ChronId, key = true, hidden = true),
      SqlField("timestamp", ProposalStatusChangeView.Timestamp),
      SqlField("status",    ProposalStatusChangeView.ProposalStatus)
    )
