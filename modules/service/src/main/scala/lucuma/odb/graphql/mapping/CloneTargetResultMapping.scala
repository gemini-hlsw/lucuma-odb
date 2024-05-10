// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import lucuma.odb.graphql.table.TargetPairsView
import lucuma.odb.graphql.table.TargetView

trait CloneTargetResultMapping[F[_]] extends ResultMapping[F] with TargetView[F] with TargetPairsView[F] {

  lazy val CloneTargetResultMapping: ObjectMapping =
    ObjectMapping(CloneTargetResultType)(
      SqlField("synthetic-id", TargetPairsView.Left, key = true, hidden = true),
      SqlObject("originalTarget", Join(TargetPairsView.Left, TargetView.TargetId)),
      SqlObject("newTarget", Join(TargetPairsView.Right, TargetView.TargetId)),
    )

}
