// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Path

import table.DatasetTable
import table.ObservationView
import table.ExecutionEventTable
import table.StepView
import table.VisitTable

trait ExecutionEventSelectResultMapping[F[_]]
  extends ExecutionEventTable[F]
     with DatasetTable[F]
     with ObservationView[F]
     with ResultMapping[F]
     with StepView[F]
     with VisitTable[F] {

  lazy val ExecutionEventSelectResultMapping: List[TypeMapping] = {

    def fromDatasetAtPath(path: Path): ObjectMapping =
      nestedSelectResultMappingAtPath(path, DatasetTable.Id,    Join(DatasetTable.Id,    ExecutionEventTable.DatasetId))

    def fromExecutionAtPath(path: Path): ObjectMapping =
      nestedSelectResultMappingAtPath(path, ObservationView.Id, Join(ObservationView.Id, ExecutionEventTable.ObservationId))

    def fromQueryAtPath(path: Path): ObjectMapping =
      topLevelSelectResultMappingAtPath(path)

    def fromStepRecordAtPath(path: Path): ObjectMapping =
      nestedSelectResultMappingAtPath(path, StepView.Id, Join(StepView.Id, ExecutionEventTable.StepId))

    def fromVisitAtPath(path: Path): ObjectMapping =
      nestedSelectResultMappingAtPath(path, VisitTable.Id,      Join(VisitTable.Id,      ExecutionEventTable.VisitId))

    List(
      fromDatasetAtPath(DatasetType / "events"),
      fromExecutionAtPath(ExecutionType / "events"),
      fromQueryAtPath(QueryType / "events"),
      fromStepRecordAtPath(StepRecordType / "events"),
      fromVisitAtPath(VisitType / "events")
    )

  }

}
