// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.ExecutionEventTable

trait AddSlewEventResultMapping[F[_]] extends ExecutionEventTable[F] {

  lazy val AddSlewEventResultMapping: ObjectMapping =
    ObjectMapping(AddSlewEventResultType)(
      SqlField("id", ExecutionEventTable.Id, key = true, hidden = true),
      SqlObject("event")
    )

}
