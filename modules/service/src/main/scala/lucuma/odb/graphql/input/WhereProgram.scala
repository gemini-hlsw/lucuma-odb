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
import lucuma.core.enums.Instrument
import lucuma.core.model.Program
import lucuma.core.model.Semester
import lucuma.odb.data.ProgramReference
import lucuma.odb.data.ProgramType
import lucuma.odb.data.ScienceSubtype
import lucuma.odb.graphql.binding._

object WhereProgram {

  def binding(path: Path): Matcher[Predicate] = {
    val WhereOrderProgramId               = WhereOrder.binding[Program.Id](path / "id", ProgramIdBinding)
    val WhereNameBinding                  = WhereOptionString.binding(path / "name")
    val WhereTypeBinding                  = WhereEq.binding[ProgramType](path / "programType", ProgramTypeBinding)
    val WhereInstrumentBinding            = WhereOptionEq.binding[Instrument](path / "instrument", InstrumentBinding)
    val WhereOptionEqScienceSubtype       = WhereOptionEq.binding[ScienceSubtype](path / "scienceSubtype", ScienceSubtypeBinding)
    val WhereOptionOrderSemester          = WhereOptionOrder.binding[Semester](path / "semester", SemesterBinding)
    val WhereOptionOrderSemesterIndex     = WhereOptionOrder.binding[PosInt](path / "semesterIndex", PosIntBinding)
    val WhereOptionOrderProgramReference  = WhereOptionOrder.binding[ProgramReference](path / "programReference", ProgramReferenceBinding)
    val WhereEqProposalStatus             = WhereUnorderedTag.binding(path / "proposalStatus", TagBinding)
    val WhereProposalBinding              = WhereProposal.binding(path / "proposal")

    lazy val WhereProgramBinding = binding(path)

    ObjectFieldsBinding.rmap {
      case List(
        WhereProgramBinding.List.Option("AND", rAND),
        WhereProgramBinding.List.Option("OR", rOR),
        WhereProgramBinding.Option("NOT", rNOT),
        WhereOrderProgramId.Option("id", rId),
        WhereNameBinding.Option("name", rName),
        WhereTypeBinding.Option("programType", rType),
        WhereInstrumentBinding.Option("instrument", rInstrument),
        WhereOptionEqScienceSubtype.Option("scienceSubtype", rScience),
        WhereOptionOrderSemester.Option("semester", rSemester),
        WhereOptionOrderSemesterIndex.Option("semesterIndex", rSemesterIndex),
        WhereOptionOrderProgramReference.Option("programReference", rRef),
        WhereEqProposalStatus.Option("proposalStatus", rPs),
        WhereProposalBinding.Option("proposal", rPro)
      ) =>
          (rAND, rOR, rNOT, rId, rName, rType, rInstrument, rScience, rSemester, rSemesterIndex, rRef, rPs, rPro).parMapN {
            (AND, OR, NOT, id, name, ptype, instrument, science, semester, semesterIndex, ref, ps, pro) =>
              and(List(
                AND.map(and),
                OR.map(or),
                NOT.map(Not(_)),
                id,
                name,
                ptype,
                instrument,
                science,
                semester,
                semesterIndex,
                ref,
                ps,
                pro
              ).flatten)
        }
    }
  }

}
