// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all.*
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ProgramType
import lucuma.core.model.Program
import lucuma.odb.graphql.binding.*

object WhereProgram {

  def binding(path: Path): Matcher[Predicate] = {
    val WhereOrderProgramId          = WhereOrder.binding[Program.Id](path / "id", ProgramIdBinding)
    val WhereNameBinding             = WhereOptionString.binding(path / "name")
    val WhereTypeBinding             = WhereEq.binding[ProgramType](path / "type", ProgramTypeBinding)
    val WhereProgramReferenceBinding = WhereProgramReference.binding(path / "reference")
    val WherePiBinding               = WhereProgramUser.binding(path / "pi")
    val WhereEqProposalStatus        = WhereUnorderedTag.binding(path / "proposalStatus", TagBinding)
    val WhereProposalBinding         = WhereProposal.binding(path / "proposal")
    val WhereCalibrationRoleBinding  = WhereOptionEq.binding[CalibrationRole](path / "calibrationRole", enumeratedBinding[CalibrationRole])

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
        WherePiBinding.Option("pi", rPi),
        WhereEqProposalStatus.Option("proposalStatus", rPs),
        WhereProposalBinding.Option("proposal", rPro),
        WhereCalibrationRoleBinding.Option("calibrationRole", rCalibRole),
      ) =>
          (rAND, rOR, rNOT, rId, rName, rType, rRef, rPi, rPs, rPro, rCalibRole).parMapN {
            (AND, OR, NOT, id, name, ptype, ref, pi, ps, pro, calib) =>
              and(List(
                AND.map(and),
                OR.map(or),
                NOT.map(Not(_)),
                id,
                name,
                ptype,
                ref,
                pi,
                ps,
                pro,
                calib
              ).flatten)
        }
    }
  }

}
