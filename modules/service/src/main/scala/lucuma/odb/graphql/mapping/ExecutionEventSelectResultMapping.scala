// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.DatasetTable
import table.ObservationView
import table.ExecutionEventTable
import table.StepRecordView
import table.VisitTable

trait ExecutionEventSelectResultMapping[F[_]]
  extends ExecutionEventTable[F]
     with DatasetTable[F]
     with ObservationView[F]
     with ResultMapping[F]
     with StepRecordView[F]
     with VisitTable[F] {

  lazy val ExecutionEventSelectResultMapping: TypeMapping = {

    val fromDataset: ObjectMapping =
      nestedSelectResultMapping(ExecutionEventSelectResultType, DatasetTable.Id,    Join(DatasetTable.Id,    ExecutionEventTable.DatasetId))

    val fromExecution: ObjectMapping =
      nestedSelectResultMapping(ExecutionEventSelectResultType, ObservationView.Id, Join(ObservationView.Id, ExecutionEventTable.ObservationId))

    val fromQuery: ObjectMapping =
      topLevelSelectResultMapping(ExecutionEventSelectResultType)

    val fromStepRecord: ObjectMapping =
      nestedSelectResultMapping(ExecutionEventSelectResultType, StepRecordView.Id, Join(StepRecordView.Id, ExecutionEventTable.StepId))

    val fromVisit: ObjectMapping =
      nestedSelectResultMapping(ExecutionEventSelectResultType, VisitTable.Id,      Join(VisitTable.Id,      ExecutionEventTable.VisitId))

    SwitchMapping(
      ExecutionEventSelectResultType,
      List(
        DatasetType    / "events" -> fromDataset,
        ExecutionType  / "events" -> fromExecution,
        QueryType      / "events" -> fromQuery,
        StepRecordType / "events" -> fromStepRecord,
        VisitType      / "events" -> fromVisit
      )
    )
  }

}
