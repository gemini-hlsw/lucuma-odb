// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.numeric.PosInt
import grackle.Path
import grackle.Predicate
import grackle.Predicate._
import lucuma.core.model.Semester
import lucuma.odb.graphql.binding.BooleanBinding
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObjectFieldsBinding
import lucuma.odb.graphql.binding.PosIntBinding
import lucuma.odb.graphql.binding.SemesterBinding

object WhereProposalReference {

  def binding(path: Path): Matcher[Predicate] = {
    // match proposal reference labels on the String value so we can do
    // comparisons like 'LIKE: "G-2024A-%"'.
    val WhereLabel    = WhereString.binding(path / "labelString")
    val WhereSemester = WhereOrder.binding[Semester](path / "semester", SemesterBinding)
    val WhereIndex    = WhereOrder.binding[PosInt](path / "semesterIndex", PosIntBinding)

    ObjectFieldsBinding.rmap {
      case List(
        BooleanBinding.Option("IS_NULL", rIsNull),
        WhereLabel.Option("label", rRef),
        WhereSemester.Option("semester", rSemester),
        WhereIndex.Option("semesterIndex", rIndex)
      ) => (rIsNull, rRef, rSemester, rIndex).parMapN { (isNull, ref, semester, index) =>
        and(List(
          isNull.map(IsNull(path / "id", _)),
          ref,
          semester,
          index
        ).flatten)
      }
    }
  }

}
