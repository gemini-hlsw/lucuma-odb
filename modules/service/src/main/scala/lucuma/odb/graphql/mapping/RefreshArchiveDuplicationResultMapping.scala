// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import lucuma.odb.graphql.table.ArchiveDuplicationView
import lucuma.odb.graphql.table.ObservationView

trait RefreshArchiveDuplicationResultMapping[F[_]]
  extends ObservationView[F]
     with ArchiveDuplicationView[F]:

  lazy val RefreshArchiveDuplicationResultMapping: ObjectMapping =
    ObjectMapping(RefreshArchiveDuplicationResultType)(
      SqlField("id", ObservationView.Id, key = true, hidden = true),
      SqlObject("archiveDuplication", Join(ObservationView.Id, ArchiveDuplicationView.ObservationId)),
      SqlObject("observation")
    )
