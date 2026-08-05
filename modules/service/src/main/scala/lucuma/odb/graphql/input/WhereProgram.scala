// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all.*
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import java.time.LocalDate
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ProgramType
import lucuma.core.enums.ProgramUserRole
import lucuma.core.model.Program
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.binding.WhereEq
import lucuma.odb.graphql.binding.WhereOptionEq
import lucuma.odb.graphql.binding.WhereOptionString
import lucuma.odb.graphql.binding.WhereOrder
import org.typelevel.cats.time.given

object WhereProgram {

  def binding(path: Path)(using serverDate: LocalDate): Matcher[Predicate] = {
    val WhereOrderProgramId          = WhereOrder.binding[Program.Id](path / "id", ProgramIdBinding)
    val WhereNameBinding             = WhereOptionString.binding(path / "name")
    val WhereTypeBinding             = WhereEq.binding[ProgramType](path / "type", ProgramTypeBinding)
    val WhereProgramReferenceBinding = WhereProgramReference.binding(path / "reference")
    val WherePiBinding               = WhereProgramUser.binding(path / "pi", ProgramUserRole.Pi.some)
    val WhereEqProposalStatus        = WhereEq.binding(path / "proposalStatus", ProposalStatusBinding)
    val WhereProposalBinding         = WhereProposal.binding(path / "proposal")
    val WhereCalibrationRoleBinding  = WhereOptionEq.binding[CalibrationRole](path / "calibrationRole", enumeratedBinding[CalibrationRole])
    val WhereStartBinding            = WhereOrder.binding(path / "active" / "start", DateBinding)
    val WhereEndBinding              = WhereOrder.binding(path / "active" / "end",   DateBinding)

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
        WhereStartBinding.Option("activeStart", rStart),
        WhereEndBinding.Option("activeEnd", rEnd),
        BooleanBinding.Option("isActive", rIsActive)
      ) =>
          (rAND, rOR, rNOT, rId, rName, rType, rRef, rPi, rPs, rPro, rCalibRole, rStart, rEnd, rIsActive).parMapN {
            (AND, OR, NOT, id, name, ptype, ref, pi, ps, pro, calib, start, end, isActive) =>
              // `isActive` is resolved against the server's current date
              // (threaded request-scoped, like `user`): the program's
              // `[activeStart, activeEnd]` window must contain it. Standard
              // pushable predicates over the existing date columns.
              val activeWindow = and(List(
                LtEql(path / "active" / "start", Const(serverDate)),
                GtEql(path / "active" / "end",   Const(serverDate))
              ))
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
                calib,
                start,
                end,
                isActive.map(if _ then activeWindow else Not(activeWindow))
              ).flatten)
        }
    }
  }

}
