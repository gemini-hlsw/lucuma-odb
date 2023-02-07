// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import edu.gemini.grackle.Cursor
import edu.gemini.grackle.Result
import lucuma.odb.graphql.BaseMapping

import scala.tools.util.PathResolver.Environment
import lucuma.odb.graphql.table.TargetView

trait CloneTargetResultMapping[F[_]] extends ResultMapping[F] with TargetView[F] {

  lazy val CloneTargetResultMapping: ObjectMapping =
    ObjectMapping(
      tpe = CloneTargetResultType ,
      fieldMappings = List(
        SqlField("synthetic-id", TargetView.TargetId, key = true, hidden = true),
        SqlObject("originalTarget"),
        SqlObject("newTarget"),
      )
    )

}