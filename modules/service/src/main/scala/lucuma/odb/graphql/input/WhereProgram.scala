// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all._
import grackle.Path
import grackle.Predicate
import grackle.Predicate._
import lucuma.core.enums.ProgramType
import lucuma.core.model.Program
import lucuma.odb.graphql.binding._

object WhereProgram {

  def binding(path: Path): Matcher[Predicate] = {
    val WhereOrderProgramId            = WhereOrder.binding[Program.Id](path / "id", ProgramIdBinding)
    val WhereNameBinding               = WhereOptionString.binding(path / "name")
    val WhereTypeBinding               = WhereEq.binding[ProgramType](path / "type", ProgramTypeBinding)
    val WhereProgramReferenceBinding  = WhereProgramReference.binding(path / "reference")
    val WhereEqProposalStatus         = WhereUnorderedTag.binding(path / "proposalStatus", TagBinding)
    val WhereProposalBinding          = WhereProposal.binding(path / "proposal")

    lazy val WhereProgramBinding = binding(path)

    ObjectFieldsBinding.rmap {
      case List(
        WhereProgramBinding.List.Option("AND", rAND),
        WhereProgramBinding.List.Option("OR", rOR),
        WhereProgramBinding.Option("NOT", rNOT),
        WhereOrderProgramId.Option("id", rId),
        WhereNameBinding.Option("name", rName),
        WhereTypeBinding.Option("type", rType),
        WhereProgramReferenceBinding.Option("reference", rRef),
        WhereEqProposalStatus.Option("proposalStatus", rPs),
        WhereProposalBinding.Option("proposal", rPro)
      ) =>
          (rAND, rOR, rNOT, rId, rName, rType, rRef, rPs, rPro).parMapN {
            (AND, OR, NOT, id, name, ptype, ref, ps, pro) =>
              and(List(
                AND.map(and),
                OR.map(or),
                NOT.map(Not(_)),
                id,
                name,
                ptype,
                ref,
                ps,
                pro
              ).flatten)
        }
    }
  }

}
