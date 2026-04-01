// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import lucuma.odb.graphql.table.ObservationView

trait CloneObservationResultMapping[F[_]] extends ResultMapping[F] with ObservationView[F] {

  lazy val CloneObservationResultMapping: ObjectMapping =
    ObjectMapping(CloneObservationResultType)(
      SqlField("synthetic-id", ObservationView.Id, key = true, hidden = true),
      SqlObject("newObservation")
    )

}

