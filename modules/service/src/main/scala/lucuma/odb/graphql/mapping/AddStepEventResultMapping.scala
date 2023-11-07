// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.ExecutionEventTable

trait AddStepEventResultMapping[F[_]] extends ExecutionEventTable[F] {

  lazy val AddStepEventResultMapping: ObjectMapping =
    ObjectMapping(
      tpe = AddStepEventResultType,
      fieldMappings = List(
        SqlField("id", ExecutionEventTable.Id, key = true, hidden = true),
        SqlObject("event")
      )
    )
}
