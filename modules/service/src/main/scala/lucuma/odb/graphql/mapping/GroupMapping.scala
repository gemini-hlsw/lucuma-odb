// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import edu.gemini.grackle.Query
import edu.gemini.grackle.Query.Binding
import edu.gemini.grackle.Query.OrderBy
import edu.gemini.grackle.Query.OrderSelection
import edu.gemini.grackle.Query.OrderSelections
import edu.gemini.grackle.QueryCompiler.Elab
import edu.gemini.grackle.TypeRef
import edu.gemini.grackle.skunk.SkunkMapping
import eu.timepit.refined.types.numeric.NonNegShort
import lucuma.odb.graphql.table.GroupElementView
import lucuma.odb.graphql.table.GroupView
import lucuma.odb.graphql.table.ProgramTable

trait GroupMapping[F[_]] extends GroupView[F] with ProgramTable[F] with GroupElementView[F] {

  lazy val GroupMapping =
    ObjectMapping(
      tpe = GroupType,
      fieldMappings = List(
        SqlField("id", GroupView.Id, key = true),
        SqlField("parentId", GroupView.ParentId),
        SqlField("parentIndex", GroupView.ParentIndex),
        SqlField("name", GroupView.Name),
        SqlField("description", GroupView.Description),
        SqlField("minimumRequired", GroupView.MinRequired),
        SqlField("ordered", GroupView.Ordered),
        SqlObject("minimumInterval"),
        SqlObject("maximumInterval"),
        SqlObject("elements", Join(GroupView.Id, GroupElementView.GroupId)),
      )
    )

  lazy val GroupElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (GroupType, "elements", Nil) =>
      Elab.transformChild { child =>
        OrderBy(OrderSelections(List(OrderSelection[NonNegShort](GroupElementType / "parentIndex"))), child)
      }

}

