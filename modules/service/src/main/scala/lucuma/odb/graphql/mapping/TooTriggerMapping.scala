// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import lucuma.odb.graphql.table.ObservationView
import lucuma.odb.graphql.table.TooTriggerTable
import lucuma.odb.graphql.table.UserTable

trait TooTriggerMapping[F[_]] extends TooTriggerTable[F] with ObservationView[F] with UserTable[F]:

  lazy val TooTriggerMapping: ObjectMapping =
    ObjectMapping(TooTriggerType)(
      SqlField("id", TooTriggerTable.Id, key = true),
      SqlField("programId", TooTriggerTable.ProgramId, hidden = true),
      SqlObject("observation", Join(TooTriggerTable.ObservationId, ObservationView.Id)),
      SqlField("status", TooTriggerTable.Status),
      SqlField("resolutionReason", TooTriggerTable.ResolutionReason),
      SqlField("requestedAt", TooTriggerTable.RequestedAt),
      SqlObject("requestedBy", Join(TooTriggerTable.RequestedBy, UserTable.UserId)),
      SqlField("updatedAt", TooTriggerTable.UpdatedAt)
    )
