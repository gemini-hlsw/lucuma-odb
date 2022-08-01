// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package snippet
package input

import cats.syntax.all._
import edu.gemini.grackle.Path.UniquePath
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Predicate._
import lucuma.odb.graphql.util.Bindings._

object WhereProgram {

  private val NameBinding = WhereOptionString.binding(UniquePath(List("name")))

  val Binding: Matcher[Predicate] =
    ObjectFieldsBinding.rmap {
      case List(
        WhereProgram.Binding.List.Option("AND", rAND),
        WhereProgram.Binding.List.Option("OR", rOR),
        WhereProgram.Binding.Option("NOT", rNOT),
        WhereOrderProgramId.Binding.Option("id", rId),
        NameBinding.Option("name", rName),
        ("proposal", _), // ignore for now
      ) =>
        (rAND, rOR, rNOT, rId, rName).parMapN { (AND, OR, NOT, id, name) =>
          and(List(
            AND.map(and),
            OR.map(or),
            NOT.map(Not(_)),
            id,
            name
          ).flatten)
        }
    }

}