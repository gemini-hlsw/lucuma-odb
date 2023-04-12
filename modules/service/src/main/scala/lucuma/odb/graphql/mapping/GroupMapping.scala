// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import edu.gemini.grackle.skunk.SkunkMapping

import table.ObservationView
import lucuma.odb.graphql.table.GroupView
import lucuma.odb.graphql.table.ProgramTable

trait GroupMapping[F[_]] extends GroupView[F] with ProgramTable[F] {

  lazy val GroupMapping =
    ObjectMapping(
      tpe = GroupType,
      fieldMappings = List(
        SqlField("id", GroupView.Id, key = true),
        SqlObject("program", Join(GroupView.ProgramId, ProgramTable.Id)),
        SqlObject("parentGroup", Join(GroupView.ParentId, GroupView.Id)),
        SqlField("parentIndex", GroupView.ParentIndex),
        SqlField("name", GroupView.Name),
        SqlField("description", GroupView.Description),
        SqlField("minimumRequired", GroupView.MinRequired),
        SqlField("ordered", GroupView.Ordered),
        SqlObject("minimumInterval"),
        SqlObject("maximumInterval"),
        // SqlObject("elements", Join(GroupView.Id, GroupElementView.GroupId)),
      )
    )

}

