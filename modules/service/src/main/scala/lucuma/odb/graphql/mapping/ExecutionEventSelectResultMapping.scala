// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.DatasetTable
import table.ObservationView
import table.ExecutionEventView
import table.StepRecordTable
import table.VisitTable

trait ExecutionEventSelectResultMapping[F[_]]
  extends ExecutionEventView[F]
     with DatasetTable[F]
     with ObservationView[F]
     with ResultMapping[F]
     with StepRecordTable[F]
     with VisitTable[F] {

  lazy val ExecutionEventSelectResultMapping: TypeMapping =
    SwitchMapping(
      ExecutionEventSelectResultType,
      List(
        DatasetType / "events"    -> nestedSelectResultMapping(ExecutionEventSelectResultType, DatasetTable.Id,    Join(DatasetTable.Id,    ExecutionEventView.DatasetId)),
        ExecutionType / "events"  -> nestedSelectResultMapping(ExecutionEventSelectResultType, ObservationView.Id, Join(ObservationView.Id, ExecutionEventView.ObservationId)),
        StepRecordType / "events" -> nestedSelectResultMapping(ExecutionEventSelectResultType, StepRecordTable.Id, Join(StepRecordTable.Id, ExecutionEventView.StepId)),
        VisitType / "events"      -> nestedSelectResultMapping(ExecutionEventSelectResultType, VisitTable.Id,      Join(VisitTable.Id,      ExecutionEventView.VisitId))
      )
    )

}
