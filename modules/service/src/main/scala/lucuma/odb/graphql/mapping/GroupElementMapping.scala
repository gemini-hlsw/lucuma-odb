// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping
import lucuma.odb.graphql.table.GroupElementView
import lucuma.odb.graphql.table.GroupView

import table.ObservationView

trait GroupElementMapping[F[_]] extends GroupElementView[F] with GroupView[F] with ObservationView[F] {

  lazy val GroupElementMapping =
    ObjectMapping(GroupElementType)(
      SqlField("id", GroupElementView.Id, key = true, hidden = true),
      SqlField("programId", GroupElementView.ProgramId, hidden = true),
      SqlField("parentGroupId", GroupElementView.GroupId),
      SqlField("parentIndex", GroupElementView.Index),
      SqlObject("group", Join(GroupElementView.ChildGroupId, GroupView.Id)),
      SqlObject("observation", Join(GroupElementView.ChildObservationId, ObservationView.Id)),
      SqlField("existence", GroupElementView.Existence),
    )

}

