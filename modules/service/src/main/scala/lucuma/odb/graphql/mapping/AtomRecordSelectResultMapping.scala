// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Path

import table.AtomTable
import table.ObservationView
import table.VisitTable

trait AtomRecordSelectResultMapping[F[_]]
  extends AtomTable[F]
     with ObservationView[F]
     with ResultMapping[F]
     with VisitTable[F] {

  lazy val AtomRecordSelectResultMappings: List[TypeMapping] = {

    def fromExecutionAtPath(path: Path): ObjectMapping =
      nestedSelectResultMappingAtPath(path, ObservationView.Id, Join(ObservationView.Id, AtomTable.ObservationId))

    def fromVisitAtPath(path: Path): ObjectMapping =
      nestedSelectResultMappingAtPath(path, VisitTable.Id, Join(VisitTable.Id, AtomTable.VisitId))

    List(
      fromExecutionAtPath(ExecutionType / "atomRecords"),
      fromVisitAtPath(VisitType     / "atomRecords")
    )

  }

}
