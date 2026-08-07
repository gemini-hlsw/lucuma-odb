// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import lucuma.odb.graphql.table.TooTriggerTable

// Declining is the only single-trigger mutation; requesting and withdrawing
// happen through the observation's workflow state.
trait TooTriggerResultMappings[F[_]] extends TooTriggerTable[F]:

  lazy val DeclineTooTriggerResultMapping =
    ObjectMapping(DeclineTooTriggerResultType)(
      SqlField("id", TooTriggerTable.Id, key = true, hidden = true),
      SqlObject("tooTrigger")
    )
