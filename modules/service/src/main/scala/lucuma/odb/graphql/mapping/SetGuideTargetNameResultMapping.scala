// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import lucuma.odb.graphql.table.ObservationView

trait SetGuideTargetNameResultMapping[F[_]] extends ObservationView[F] {

  lazy val SetGuideTargetNameResultMapping =
    ObjectMapping(SetGuideTargetNameResultType)(
      SqlField("observationId", ObservationView.Id, key = true, hidden = true),
      SqlObject("observation")
    )
}
