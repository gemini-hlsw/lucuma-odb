// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import lucuma.odb.graphql.table.GoaDuplicationView
import lucuma.odb.graphql.table.ObservationView

trait RefreshGoaDuplicationResultMapping[F[_]]
  extends ObservationView[F]
     with GoaDuplicationView[F]:

  lazy val RefreshGoaDuplicationResultMapping: ObjectMapping =
    ObjectMapping(RefreshGoaDuplicationResultType)(
      SqlField("id", ObservationView.Id, key = true, hidden = true),
      SqlObject("goaDuplication", Join(ObservationView.Id, GoaDuplicationView.ObservationId)),
      SqlObject("observation")
    )
