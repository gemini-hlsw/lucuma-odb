// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.AtomRecordView
import table.StepRecordView

trait StepRecordSelectResultMapping[F[_]]
  extends AtomRecordView[F[_]]
     with ResultMapping[F]
     with StepRecordView[F] {

  lazy val StepRecordSelectResultMapping: TypeMapping =
    nestedSelectResultMapping(StepRecordSelectResultType, AtomRecordView.Id, Join(AtomRecordView.Id, StepRecordView.AtomId))

}
