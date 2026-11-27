// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping
import lucuma.odb.graphql.table.GroupDescendantElementView
import lucuma.odb.graphql.table.GroupElementView
import lucuma.odb.graphql.table.GroupView

import table.ObservationView

trait GroupElementMapping[F[_]] extends GroupElementView[F] with GroupDescendantElementView[F] with GroupView[F] with ObservationView[F] {

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

  // Group.allElements reads the view that pairs a group with everything under it.
  lazy val GroupAllElementsMapping =
    ObjectMapping(GroupType / "allElements")(
      SqlField("id", GroupDescendantElementView.Id, key = true, hidden = true),
      SqlField("ancestorId", GroupDescendantElementView.AncestorId, hidden = true),
      SqlField("programId", GroupDescendantElementView.ProgramId, hidden = true),
      SqlField("parentGroupId", GroupDescendantElementView.GroupId),
      SqlField("parentIndex", GroupDescendantElementView.Index),
      SqlObject("group", Join(GroupDescendantElementView.ChildGroupId, GroupView.Id)),
      SqlObject("observation", Join(GroupDescendantElementView.ChildObservationId, ObservationView.Id)),
      SqlField("existence", GroupDescendantElementView.Existence),
    )

}

