// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import lucuma.odb.graphql.table.ObservationView
import lucuma.odb.graphql.table.VisitTable

trait VisitSelectResultMapping[F[_]]
  extends VisitTable[F]
     with ObservationView[F]
     with ResultMapping[F] {

  lazy val VisitSelectResultMapping: TypeMapping =
    nestedSelectResultMapping(VisitSelectResultType, ObservationView.Id, Join(ObservationView.Id, VisitTable.ObservationId))

}
