// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.parallel._
import edu.gemini.grackle.Path
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Predicate._
import lucuma.core.enums.ObsActiveStatus
import lucuma.core.enums.ObsStatus
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.odb.graphql.binding._
import lucuma.odb.graphql.mapping.TargetMapping

object WhereTarget {

  def binding(path: Path): Matcher[Predicate] = {
    val WhereOrderTargetIdBinding = WhereOrder.binding[Target.Id](path / "id", TargetIdBinding)
    val WhereOrderProgramIdBinding = WhereOrder.binding[Program.Id](path / "program" / "id", ProgramIdBinding)
    val WhereNameBinding = WhereString.binding(path / "name")
    lazy val WhereTargetBinding = binding(path)
    ObjectFieldsBinding.rmap {
      case List(
        WhereTargetBinding.List.Option("AND", rAND),
        WhereTargetBinding.List.Option("OR", rOR),
        WhereTargetBinding.Option("NOT", rNOT),        
        WhereOrderTargetIdBinding.Option("id", rId),
        WhereOrderProgramIdBinding.Option("programId", rPid),
        WhereNameBinding.Option("name", rName),
      ) =>
        (rAND, rOR, rNOT, rId, rPid, rName).parMapN { (AND, OR, NOT, id, pid, name) =>
          and(List(
            AND.map(and),
            OR.map(or),
            NOT.map(Not(_)),
            id,
            pid,
            name,
          ).flatten)
        }
    }
  }

}

