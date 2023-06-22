// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.DatasetEventTable

trait DatasetIdMapping[F[_]] extends DatasetEventTable[F] {

  lazy val DatasetIdMapping: ObjectMapping =
    ObjectMapping(
      tpe = DatasetIdType,
      fieldMappings = List(
        SqlField("id", DatasetEventTable.Id, hidden = true, key = true),
        SqlField("stepId", DatasetEventTable.DatasetId.StepId),
        SqlField("index",  DatasetEventTable.DatasetId.Index)
      )
    )

}
