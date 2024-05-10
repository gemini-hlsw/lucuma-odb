// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.ExecutionEventTable

trait AddSequenceEventResultMapping[F[_]] extends ExecutionEventTable[F] {

  lazy val AddSequenceEventResultMapping: ObjectMapping =
    ObjectMapping(AddSequenceEventResultType)(
      SqlField("id", ExecutionEventTable.Id, key = true, hidden = true),
      SqlObject("event")
    )

}
