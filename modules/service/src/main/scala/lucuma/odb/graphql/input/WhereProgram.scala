// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all._
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.numeric.PosInt
import grackle.Path
import grackle.Predicate
import grackle.Predicate._
import lucuma.core.model.Program
import lucuma.core.model.Semester
import lucuma.odb.data.ProposalReference
import lucuma.odb.graphql.binding._

object WhereProgram {

  def binding(path: Path): Matcher[Predicate] = {
    val WhereOrderProgramId = WhereOrder.binding[Program.Id](path / "id", ProgramIdBinding)
    val WhereNameBinding = WhereOptionString.binding(path / "name")
    val WhereOptionOrderSemester = WhereOptionOrder.binding[Semester](path / "semester", SemesterBinding)
    val WhereOrderSemesterIndex = WhereOrder.binding[PosInt](path / "semesterIndex", PosIntBinding)
    val WhereOrderProposalReference = WhereOrder.binding[ProposalReference](path / "proposalReference", ProposalReferenceBinding)
    val WhereEqProposalStatus = WhereUnorderedTag.binding(path / "proposalStatus", TagBinding)
    lazy val WhereProgramBinding = binding(path)
    ObjectFieldsBinding.rmap {
      case List(
        WhereProgramBinding.List.Option("AND", rAND),
        WhereProgramBinding.List.Option("OR", rOR),
        WhereProgramBinding.Option("NOT", rNOT),
        WhereOrderProgramId.Option("id", rId),
        WhereNameBinding.Option("name", rName),
        WhereOptionOrderSemester.Option("semester", rSemester),
        WhereOrderSemesterIndex.Option("semesterIndex", rSemesterIndex),
        WhereOrderProposalReference.Option("proposalReference", rProposalReference),
        WhereEqProposalStatus.Option("proposalStatus", rPs),
        ("proposal", _), // ignore for now
      ) =>
          (rAND, rOR, rNOT, rId, rName, rSemester, rSemesterIndex, rProposalReference, rPs).parMapN { (AND, OR, NOT, id, name, semester, semesterIndex, proposalReference, ps) =>
          and(List(
            AND.map(and),
            OR.map(or),
            NOT.map(Not(_)),
            id,
            name,
            semester,
            semesterIndex,
            proposalReference,
            ps
          ).flatten)
        }
    }
  }

}
