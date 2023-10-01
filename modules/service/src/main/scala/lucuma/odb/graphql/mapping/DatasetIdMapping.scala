// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.DatasetTable

trait DatasetIdMapping[F[_]] extends DatasetTable[F] {

  lazy val DatasetIdMapping: ObjectMapping =
    ObjectMapping(
      tpe = DatasetIdType,
      fieldMappings = List(
        SqlField("stepId", DatasetTable.DatasetId.StepId, key = true),
        SqlField("index",  DatasetTable.DatasetId.Index, key = true)
      )
    )

}
