// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all._
import grackle.Path
import grackle.Predicate
import grackle.Predicate._
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.odb.graphql.binding._

object WhereTargetInput {

  def binding(path: Path): Matcher[Predicate] = {
    val WhereNameBinding = WhereString.binding(path / "name")
    val WhereProgramIdBinding = WhereOrder.binding[Program.Id](path / "program" / "id", ProgramIdBinding)
    val WhereTargetIdBinding = WhereOrder.binding[Target.Id](path / "id", TargetIdBinding)
    lazy val WhereTargetInputBinding = binding(path)
    ObjectFieldsBinding.rmap {
      case List(
        WhereTargetInputBinding.List.Option("AND", rAND),
        WhereTargetInputBinding.List.Option("OR", rOR),
        WhereTargetInputBinding.Option("NOT", rNOT),
        WhereTargetIdBinding.Option("id", rId),
        WhereProgramIdBinding.Option("programId", rProgramId),
        WhereNameBinding.Option("name", rName),
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

}