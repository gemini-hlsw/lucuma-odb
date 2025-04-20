// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import lucuma.core.enums.ScienceSubtype
import lucuma.core.model.Semester
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObjectFieldsBinding
import lucuma.odb.graphql.binding.ScienceSubtypeBinding
import lucuma.odb.graphql.binding.SemesterBinding

case class ProgramReferencePropertiesScienceInput(
  semester:       Semester,
  scienceSubtype: ScienceSubtype
)

object ProgramReferencePropertiesScienceInput {

  val Binding: Matcher[ProgramReferencePropertiesScienceInput] =
    ObjectFieldsBinding.rmap {
      case List(
        SemesterBinding("semester", rSemester),
        ScienceSubtypeBinding("scienceSubtype", rScienceSubtype)
      ) => (rSemester, rScienceSubtype).parMapN(apply)
    }

}