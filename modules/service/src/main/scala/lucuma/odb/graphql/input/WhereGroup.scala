// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.parallel.*
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.core.model.Group
import lucuma.odb.graphql.binding.*

object WhereGroup {

  def binding(path: Path): Matcher[Predicate] = {
    val WhereOrderGroupIdBinding = WhereOrder.binding[Group.Id](path / "id", GroupIdBinding)
    val NameBinding = WhereOptionString.binding(path / "name")
    val DescriptionBinding = WhereOptionString.binding(path / "description")
    lazy val WhereGroupBinding = binding(path)
    ObjectFieldsBinding.rmap {
      case List(
        WhereGroupBinding.List.Option("AND", rAND),
        WhereGroupBinding.List.Option("OR", rOR),
        WhereGroupBinding.Option("NOT", rNOT),
        WhereOrderGroupIdBinding.Option("id", rId),
        NameBinding.Option("name", rName),
        DescriptionBinding.Option("description", rDescription),
      ) =>
        (rAND, rOR, rNOT, rId, rName, rDescription).parMapN { (AND, OR, NOT, id, name, description) =>
          and(List(
            AND.map(and),
            OR.map(or),
            NOT.map(Not(_)),
            id,
            name,
            description,
          ).flatten)
        }
    }
  }

}

