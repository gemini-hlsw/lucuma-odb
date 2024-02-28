// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.parallel._
import grackle.Path
import grackle.Predicate
import grackle.Predicate._
import lucuma.core.model.Target
import lucuma.odb.graphql.binding._

object WhereTarget {

  def binding(path: Path): Matcher[Predicate] = {
    val WhereOrderTargetIdBinding = WhereOrder.binding[Target.Id](path / "id", TargetIdBinding)
    val WhereProgramBinding = WhereProgram.binding(path / "program")
    val WhereNameBinding = WhereString.binding(path / "name")
    lazy val WhereTargetBinding = binding(path)
    ObjectFieldsBinding.rmap {
      case List(
        WhereTargetBinding.List.Option("AND", rAND),
        WhereTargetBinding.List.Option("OR", rOR),
        WhereTargetBinding.Option("NOT", rNOT),
        WhereOrderTargetIdBinding.Option("id", rId),
        WhereProgramBinding.Option("program", rProgram),
        WhereNameBinding.Option("name", rName),
      ) =>
        (rAND, rOR, rNOT, rId, rProgram, rName).parMapN { (AND, OR, NOT, id, program, name) =>
          and(List(
            AND.map(and),
            OR.map(or),
            NOT.map(Not(_)),
            id,
            program,
            name,
          ).flatten)
        }
    }
  }

}

