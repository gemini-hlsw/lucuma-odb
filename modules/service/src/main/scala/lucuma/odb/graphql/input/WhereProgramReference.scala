// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.numeric.PosInt
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.ScienceSubtype
import lucuma.core.model.Semester
import lucuma.odb.graphql.binding.BooleanBinding
import lucuma.odb.graphql.binding.InstrumentBinding
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObjectFieldsBinding
import lucuma.odb.graphql.binding.PosIntBinding
import lucuma.odb.graphql.binding.ScienceSubtypeBinding
import lucuma.odb.graphql.binding.SemesterBinding

object WhereProgramReference {

  def binding(path: Path): Matcher[Predicate] = {
    // match proposal reference labels on the String value so we can do
    // comparisons like 'LIKE: "G-2024A-%-Q"'.
    val WhereLabel       = WhereString.binding(path / "labelString")
    val WhereSemester    = WhereOrder.binding[Semester](path / "whereSemester", SemesterBinding)
    val WhereIndex       = WhereOrder.binding[PosInt](path / "whereSemesterIndex", PosIntBinding)
    val WhereInstrument  = WhereEq.binding[Instrument](path / "whereInstrument", InstrumentBinding)
    val WhereDescription = WhereString.binding(path / "whereDescription")
    val WhereScienceSub  = WhereEq.binding[ScienceSubtype](path / "whereScienceSubtype", ScienceSubtypeBinding)

    ObjectFieldsBinding.rmap {
      case List(
        BooleanBinding.Option("IS_NULL", rIsNull),
        WhereLabel.Option("label", rRef),
        WhereSemester.Option("semester", rSemester),
        WhereIndex.Option("semesterIndex", rIndex),
        WhereInstrument.Option("instrument", rInstrument),
        WhereDescription.Option("description", rDescription),
        WhereScienceSub.Option("scienceSubtype", rScienceSubtype)
      ) => (rIsNull, rRef, rSemester, rIndex, rInstrument, rDescription, rScienceSubtype).parMapN {
        (isNull, ref, semester, index, instrument, description, scienceSubtype) =>
          and(List(
            isNull.map(IsNull(path / "id", _)),
            ref,
            semester,
            index,
            instrument,
            description,
            scienceSubtype
          ).flatten)
      }
    }
  }

}
