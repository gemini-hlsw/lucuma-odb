// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import lucuma.odb.graphql.table.ObservationPairsView
import lucuma.odb.graphql.table.ObservationView

trait CloneObservationResultMapping[F[_]] extends ResultMapping[F] with ObservationView[F] with ObservationPairsView[F] {

  lazy val CloneObservationResultMapping: ObjectMapping =
    ObjectMapping(CloneObservationResultType)(
      SqlField("synthetic-id-1", ObservationPairsView.Left, key = true, hidden = true),
      SqlField("synthetic-id-2", ObservationPairsView.Right, key = true, hidden = true),
      SqlObject("originalObservation", Join(ObservationPairsView.Left, ObservationView.Id)),
      SqlObject("newObservation", Join(ObservationPairsView.Right, ObservationView.Id)),
    )

}
