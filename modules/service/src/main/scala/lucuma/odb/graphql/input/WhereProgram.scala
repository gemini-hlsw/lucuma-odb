// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all._
import edu.gemini.grackle.Path
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Predicate._
import lucuma.core.model.Program
import lucuma.odb.graphql.binding._

object WhereProgram {

  def binding(path: Path): Matcher[Predicate] = {
    val WhereNameBinding = WhereOptionString.binding(path / "name")
    val WhereOrderProgramId = WhereOrder.binding[Program.Id](path / "id", ProgramIdBinding)
    lazy val WhereProgramBinding = binding(path)
    ObjectFieldsBinding.rmap {
      case List(
        WhereProgramBinding.List.Option("AND", rAND),
        WhereProgramBinding.List.Option("OR", rOR),
        WhereProgramBinding.Option("NOT", rNOT),
        WhereOrderProgramId.Option("id", rId),
        WhereNameBinding.Option("name", rName),
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

}