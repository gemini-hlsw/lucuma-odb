// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.AtomRecordTable
import table.StepRecordView

trait StepRecordSelectResultMapping[F[_]]
  extends AtomRecordTable[F[_]]
     with ResultMapping[F]
     with StepRecordView[F] {

  lazy val StepRecordSelectResultMapping: TypeMapping =
    nestedSelectResultMapping(StepRecordSelectResultType, AtomRecordTable.Id, Join(AtomRecordTable.Id, StepRecordView.AtomId))

}
