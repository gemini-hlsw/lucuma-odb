// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.ExecutionEventTable

trait AddDatasetEventResultMapping[F[_]] extends ExecutionEventTable[F] {

  lazy val AddDatasetEventResultMapping: ObjectMapping =
    ObjectMapping(AddDatasetEventResultType)(
      SqlField("id", ExecutionEventTable.Id, key = true, hidden = true),
      SqlObject("event")
    )

}
