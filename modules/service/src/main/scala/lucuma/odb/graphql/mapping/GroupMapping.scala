// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import edu.gemini.grackle.Query
import edu.gemini.grackle.Query.FilterOrderByOffsetLimit
import edu.gemini.grackle.Query.OrderBy
import edu.gemini.grackle.Query.OrderSelection
import edu.gemini.grackle.Query.OrderSelections
import edu.gemini.grackle.Query.Select
import edu.gemini.grackle.Result
import edu.gemini.grackle.TypeRef
import edu.gemini.grackle.skunk.SkunkMapping
import eu.timepit.refined.types.numeric.NonNegShort
import lucuma.odb.graphql.table.GroupElementView
import lucuma.odb.graphql.table.GroupView
import lucuma.odb.graphql.table.ProgramTable

import table.ObservationView

trait GroupMapping[F[_]] extends GroupView[F] with ProgramTable[F] with GroupElementView[F] {

  lazy val GroupMapping =
    ObjectMapping(
      tpe = GroupType,
      fieldMappings = List(
        SqlField("id", GroupView.Id, key = true),
        SqlField("parentId", GroupView.ParentId, hidden = true),
        SqlField("parentIndex", GroupView.ParentIndex, hidden = true),
        SqlField("name", GroupView.Name),
        SqlField("description", GroupView.Description),
        SqlField("minimumRequired", GroupView.MinRequired),
        SqlField("ordered", GroupView.Ordered),
        SqlObject("minimumInterval"),
        SqlObject("maximumInterval"),
        SqlObject("elements", Join(GroupView.Id, GroupElementView.GroupId)),
      )
    )

  lazy val GroupElaborator: Map[TypeRef, PartialFunction[Select, Result[Query]]] =
    Map(
      GroupType -> {
        case Select("elements", Nil, child) =>
          Result(
            Select("elements", Nil,
              OrderBy(OrderSelections(List(OrderSelection[NonNegShort](GroupElementType / "parentIndex"))), child)
            )
          )
      }
    )

}

