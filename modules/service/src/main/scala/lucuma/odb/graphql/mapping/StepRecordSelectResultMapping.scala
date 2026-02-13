// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.AtomTable
import table.StepView

trait StepRecordSelectResultMapping[F[_]]
  extends AtomTable[F[_]]
     with ResultMapping[F]
     with StepView[F] {

  lazy val StepRecordSelectResultMapping: TypeMapping =
    nestedSelectResultMapping(StepRecordSelectResultType, AtomTable.Id, Join(AtomTable.Id, StepView.AtomId))

}
