// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.AtomRecordTable
import table.ObservationView
import table.VisitTable

trait AtomRecordSelectResultMapping[F[_]]
  extends AtomRecordTable[F[_]]
     with ObservationView[F]
     with ResultMapping[F]
     with VisitTable[F] {

  lazy val AtomRecordSelectResultMapping: TypeMapping =
    SwitchMapping(
      AtomRecordSelectResultType,
      List(
        ExecutionType / "atomRecords" -> nestedSelectResultMapping(AtomRecordSelectResultType, ObservationView.Id, Join(ObservationView.Id, AtomRecordTable.ObservationId)),
        VisitType / "atomRecords"     -> nestedSelectResultMapping(AtomRecordSelectResultType, VisitTable.Id, Join(VisitTable.Id, AtomRecordTable.VisitId))
      )
    )

}
