// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.AtomRecordTable
import table.ObservationView
import table.VisitTable

trait AtomRecordSelectResultMapping[F[_]]
  extends AtomRecordTable[F]
     with ObservationView[F]
     with ResultMapping[F]
     with VisitTable[F] {

  lazy val AtomRecordSelectResultMappings: List[TypeMapping] = {

    val fromExecution: ObjectMapping =
      nestedSelectResultMapping(AtomRecordSelectResultType, ObservationView.Id, Join(ObservationView.Id, AtomRecordTable.ObservationId))

    val fromVisit: ObjectMapping =
      nestedSelectResultMapping(AtomRecordSelectResultType, VisitTable.Id, Join(VisitTable.Id, AtomRecordTable.VisitId))

    SwitchMapping(
      AtomRecordSelectResultType,
      List(
        ExecutionType / "atomRecords" -> fromExecution,
        VisitType     / "atomRecords" -> fromVisit
      )
    )
  }

}
