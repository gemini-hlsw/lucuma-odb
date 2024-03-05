// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.AtomRecordTable
import table.DatasetTable
import table.ObservationView
import table.StepRecordView
import table.VisitTable

trait DatasetSelectResultMapping[F[_]]
  extends DatasetTable[F]
     with AtomRecordTable[F]
     with LookupFrom[F]
     with ObservationView[F]
     with ResultMapping[F]
     with StepRecordView[F]
     with VisitTable[F] {

  lazy val DatasetSelectResultMapping: TypeMapping = {

    val fromExecution: ObjectMapping =
      nestedSelectResultMapping(DatasetSelectResultType, ObservationView.Id, Join(ObservationView.Id, DatasetTable.ObservationId))

    val fromStepRecord: ObjectMapping =
      nestedSelectResultMapping(DatasetSelectResultType, StepRecordView.Id,  Join(StepRecordView.Id, DatasetTable.StepId))

    val fromVisit: ObjectMapping =
      nestedSelectResultMapping(DatasetSelectResultType, VisitTable.Id,      Join(VisitTable.Id, DatasetTable.VisitId))

    SwitchMapping(
      DatasetSelectResultType,
      List(
        QueryType      / "datasets" -> topLevelSelectResultMapping(DatasetSelectResultType),
        ExecutionType  / "datasets" -> fromExecution,
        StepRecordType / "datasets" -> fromStepRecord
      ) ++
      lookupFromVisit(fromVisit, "datasets")
    )
  }

}
