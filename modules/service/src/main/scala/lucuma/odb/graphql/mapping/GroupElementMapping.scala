// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.odb.graphql.table.GroupElementView
import lucuma.odb.graphql.table.GroupView
import lucuma.odb.graphql.table.ObservationView

import table.ObservationView

trait GroupElementMapping[F[_]] extends GroupElementView[F] with GroupView[F] with ObservationView[F] {

  lazy val GroupElementMapping =
    ObjectMapping(
      tpe = GroupElementType,
      fieldMappings = List(
        SqlField("groupId", GroupElementView.GroupId, key = true, hidden = true),
        SqlField("index", GroupElementView.Index, key = true, hidden = true),
        SqlObject("group", Join(GroupElementView.ChildGroupId, GroupView.Id)),
        SqlObject("observation", Join(GroupElementView.ChildObservationId, ObservationView.Id)),
      )
    )

}

