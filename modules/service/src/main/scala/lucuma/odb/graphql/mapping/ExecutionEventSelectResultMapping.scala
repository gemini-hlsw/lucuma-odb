// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.ObservationView
import table.ExecutionEventView

trait ExecutionEventSelectResultMapping[F[_]]
  extends ExecutionEventView[F]
     with ObservationView[F]
     with ResultMapping[F] {

  lazy val ExecutionEventSelectResultMapping: TypeMapping =
    nestedSelectResultMapping(ExecutionEventSelectResultType, ObservationView.Id, Join(ObservationView.Id, ExecutionEventView.ObservationId))

}
