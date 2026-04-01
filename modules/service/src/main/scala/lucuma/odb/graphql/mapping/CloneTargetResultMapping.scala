// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import lucuma.odb.graphql.table.TargetView

trait CloneTargetResultMapping[F[_]] extends ResultMapping[F] with TargetView[F] {

  lazy val CloneTargetResultMapping: ObjectMapping =
    ObjectMapping(CloneTargetResultType)(
      SqlField("synthetic-id", TargetView.TargetId, key = true, hidden = true),
      SqlObject("newTarget"),
    )

}
