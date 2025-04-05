// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import lucuma.odb.graphql.table.ObservationView

trait ResetAcquisitionResultMapping[F[_]] extends ResultMapping[F] with ObservationView[F]:

  lazy val ResetAcquisitionResultMapping: ObjectMapping =
    ObjectMapping(ResetAcquisitionResultType)(
      SqlField("id", ObservationView.Id, key = true, hidden = true),
      SqlObject("observation")
    )