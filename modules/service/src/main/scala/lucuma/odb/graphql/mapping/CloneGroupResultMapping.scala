// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import lucuma.odb.graphql.table.GroupPairsView
import lucuma.odb.graphql.table.GroupView

trait CloneGroupResultMapping[F[_]] extends ResultMapping[F] with GroupView[F] with GroupPairsView[F] {

  lazy val CloneGroupResultMapping: ObjectMapping =
    ObjectMapping(CloneGroupResultType)(
      SqlField("synthetic-id-1", GroupPairsView.Left, key = true, hidden = true),
      SqlField("synthetic-id-2", GroupPairsView.Right, key = true, hidden = true),
      SqlObject("originalGroup", Join(GroupPairsView.Left, GroupView.Id)),
      SqlObject("newGroup", Join(GroupPairsView.Right, GroupView.Id)),
    )

}
