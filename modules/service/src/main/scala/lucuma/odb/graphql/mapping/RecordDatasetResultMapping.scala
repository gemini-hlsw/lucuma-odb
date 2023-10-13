// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.DatasetTable

trait RecordDatasetResultMapping[F[_]] extends DatasetTable[F] {

  lazy val RecordDatasetResultMapping: ObjectMapping =
    ObjectMapping(
      tpe = RecordDatasetResultType,
      fieldMappings = List(
        SqlField("id", DatasetTable.Id, key = true, hidden = true),
        SqlObject("dataset")
      )
    )

}
