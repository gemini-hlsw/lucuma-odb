// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Path

import table.AtomTable
import table.DatasetTable
import table.ObservationView
import table.StepView
import table.VisitTable

trait DatasetSelectResultMapping[F[_]]
  extends DatasetTable[F]
     with AtomTable[F]
     with ObservationView[F]
     with ResultMapping[F]
     with StepView[F]
     with VisitTable[F] {

  lazy val DatasetSelectResultMappings: List[TypeMapping] = {

    def fromExecutionAtPath(path: Path): ObjectMapping =
      nestedSelectResultMappingAtPath(path, ObservationView.Id, Join(ObservationView.Id, DatasetTable.ObservationId))

    def fromStepRecordAtPath(path: Path): ObjectMapping =
      nestedSelectResultMappingAtPath(path, StepView.Id, Join(StepView.Id, DatasetTable.StepId))

    def fromVisitAtPath(path: Path): ObjectMapping =
      nestedSelectResultMappingAtPath(path, VisitTable.Id, Join(VisitTable.Id, DatasetTable.VisitId))

    List(
      topLevelSelectResultMappingAtPath(QueryType / "datasets"),
      fromExecutionAtPath(ExecutionType  / "datasets"),
      fromStepRecordAtPath(StepRecordType / "datasets"),
      fromVisitAtPath(VisitType / "datasets"),
    )

  }

}
