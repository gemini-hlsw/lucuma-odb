// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.DatasetEventTable

trait AddDatasetEventResultMapping[F[_]] extends DatasetEventTable[F] {

  lazy val AddDatasetEventResultMapping: ObjectMapping =
    ObjectMapping(
      tpe = AddDatasetEventResultType,
      fieldMappings = List(
        SqlField("id", DatasetEventTable.Id, key = true, hidden = true),
        SqlObject("event")
      )
    )

}
