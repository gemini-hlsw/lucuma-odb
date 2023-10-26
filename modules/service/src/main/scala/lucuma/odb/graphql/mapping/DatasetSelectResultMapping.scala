// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.AtomRecordTable
import table.DatasetTable
import table.ObservationView
import table.StepRecordTable
import table.VisitTable

trait DatasetSelectResultMapping[F[_]]
  extends DatasetTable[F]
     with AtomRecordTable[F]
     with ObservationView[F]
     with ResultMapping[F]
     with StepRecordTable[F]
     with VisitTable[F] {

  lazy val DatasetSelectResultMapping: TypeMapping =
    SwitchMapping(
      DatasetSelectResultType,
      List(
        QueryType / "datasets"      -> topLevelSelectResultMapping(DatasetSelectResultType),
        StepRecordType / "datasets" -> nestedSelectResultMapping(DatasetSelectResultType, StepRecordTable.Id, Join(StepRecordTable.Id, DatasetTable.StepId)),
        ExecutionType / "datasets"  -> nestedSelectResultMapping(DatasetSelectResultType, ObservationView.Id, Join(ObservationView.Id, DatasetTable.ObservationId)),
        VisitType / "datasets"      -> nestedSelectResultMapping(DatasetSelectResultType, VisitTable.Id,      Join(VisitTable.Id, DatasetTable.VisitId))
      )
    )

}
