// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import edu.gemini.grackle.skunk.SkunkMapping

import table.GroupView
trait CreateGroupResultMapping[F[_]] extends GroupView[F] {

  lazy val CreateGroupResultMapping =
    ObjectMapping(
      tpe = CreateGroupResultType,
      fieldMappings = List(
        SqlField("id", GroupView.Id, key = true, hidden = true),
        SqlObject("group"),
      )
    )

}

