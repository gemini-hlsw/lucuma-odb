// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import lucuma.odb.graphql.table.ObservationView

trait UnfreezeTimeAccountingResultMapping[F[_]] extends ObservationView[F]:

  lazy val UnfreezeTimeAccountingResultMapping: ObjectMapping =
    ObjectMapping(UnfreezeTimeAccountingResultType)(
      SqlField("id", ObservationView.Id, key = true, hidden = true),
      SqlObject("observation")
    )
