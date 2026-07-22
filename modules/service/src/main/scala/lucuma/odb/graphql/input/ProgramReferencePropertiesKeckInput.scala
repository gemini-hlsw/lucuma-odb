// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import lucuma.core.model.Semester
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObjectFieldsBinding
import lucuma.odb.graphql.binding.SemesterBinding

case class ProgramReferencePropertiesKeckInput(
  semester: Semester
)

object ProgramReferencePropertiesKeckInput {

  val Binding: Matcher[ProgramReferencePropertiesKeckInput] =
    ObjectFieldsBinding.rmap {
      case List(
        SemesterBinding("semester", rSemester)
      ) => rSemester.map(apply)
    }

}
