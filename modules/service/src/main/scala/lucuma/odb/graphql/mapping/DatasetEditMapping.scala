// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import lucuma.core.model.sequence.Dataset
import lucuma.odb.data.EditType

import table.DatasetTable

trait DatasetEditMapping[F[_]] extends DatasetTable[F]:

  // N.B. env is populated by the subscription elaborator
  lazy val DatasetEditMapping =
    ObjectMapping(DatasetEditType)(
      SqlField("synthetic-id", DatasetTable.Id, key = true, hidden = true),
      CursorField("editType", _.envR[EditType]("editType"), List("synthetic-id")),
      CursorField("datasetId", _.envR[Dataset.Id]("datasetId"), List("synthetic-id")),
      SqlObject("value")
    )