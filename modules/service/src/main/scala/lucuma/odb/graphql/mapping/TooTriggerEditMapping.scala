// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import lucuma.odb.data.EditType
import lucuma.odb.graphql.table.ObservationView
import lucuma.odb.graphql.table.TooTriggerTable

trait TooTriggerEditMapping[F[_]] extends TooTriggerTable[F] with ObservationView[F]:

  // Rooted on the trigger row (triggers are never deleted, so `value` is always
  // present).  editType is populated by the subscription elaborator; `value` is the
  // trigger itself (resolved by the shared key) and `observation` its observation.
  lazy val TooTriggerEditMapping: ObjectMapping =
    ObjectMapping(TooTriggerEditType)(
      SqlField("tooTriggerId", TooTriggerTable.Id, key = true),
      CursorField("editType", _.envR[EditType]("editType"), List("tooTriggerId")),
      SqlObject("value"),
      SqlObject("observation", Join(TooTriggerTable.ObservationId, ObservationView.Id))
    )
