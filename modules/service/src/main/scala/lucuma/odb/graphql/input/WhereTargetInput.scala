// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all._
import edu.gemini.grackle.Path.UniquePath
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Predicate._
import lucuma.odb.graphql.util.Bindings._

object WhereTarget {

  private val NameBinding = WhereString.binding(UniquePath(List("name")))
  private val ProgramIdBinding = WhereOrder.ProgramIdWithPath("program", "id")

  val Binding: Matcher[Predicate] =
    ObjectFieldsBinding.rmap {
      case List(
        WhereTarget.Binding.List.Option("AND", rAND),
        WhereTarget.Binding.List.Option("OR", rOR),
        WhereTarget.Binding.Option("NOT", rNOT),
        WhereOrder.TargetId.Option("id", rId),
        WhereOrder.ProgramId.Option("programId", rProgramId),
        NameBinding.Option("name", rName),
      ) =>
        (rAND, rOR, rNOT, rId, rProgramId, rName).parMapN { (AND, OR, NOT, id, pid, name) =>
          and(List(
            AND.map(and),
            OR.map(or),
            NOT.map(Not(_)),
            id,
            pid,
            name
          ).flatten)
        }
    }

}