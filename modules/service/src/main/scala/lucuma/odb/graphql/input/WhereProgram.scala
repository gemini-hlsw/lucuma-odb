// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all._
import grackle.Path
import grackle.Predicate
import grackle.Predicate._
import lucuma.core.model.Program
import lucuma.odb.graphql.binding._

object WhereProgram {

  def binding(path: Path): Matcher[Predicate] = {
    val WhereNameBinding = WhereOptionString.binding(path / "name")
    val WhereOrderProgramId = WhereOrder.binding[Program.Id](path / "id", ProgramIdBinding)
    val WhereEqProposalStatus = WhereUnorderedTag.binding(path / "proposalStatus", TagBinding)
    lazy val WhereProgramBinding = binding(path)
    ObjectFieldsBinding.rmap {
      case List(
        WhereProgramBinding.List.Option("AND", rAND),
        WhereProgramBinding.List.Option("OR", rOR),
        WhereProgramBinding.Option("NOT", rNOT),
        WhereOrderProgramId.Option("id", rId),
        WhereNameBinding.Option("name", rName),
        WhereEqProposalStatus.Option("proposalStatus", rPs),
        ("proposal", _), // ignore for now
      ) =>
          (rAND, rOR, rNOT, rId, rName, rPs).parMapN { (AND, OR, NOT, id, name, ps) =>
          and(List(
            AND.map(and),
            OR.map(or),
            NOT.map(Not(_)),
            id,
            name,
            ps
          ).flatten)
        }
    }
  }

}
