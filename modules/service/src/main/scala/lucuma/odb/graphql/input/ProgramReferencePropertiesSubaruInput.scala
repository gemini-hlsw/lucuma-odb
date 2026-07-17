// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import lucuma.core.enums.SubaruCallForProposalsType
import lucuma.core.model.Semester
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObjectFieldsBinding
import lucuma.odb.graphql.binding.SemesterBinding
import lucuma.odb.graphql.binding.SubaruCallForProposalsTypeBinding

case class ProgramReferencePropertiesSubaruInput(
  semester:   Semester,
  subaruType: SubaruCallForProposalsType
)

object ProgramReferencePropertiesSubaruInput {

  val Binding: Matcher[ProgramReferencePropertiesSubaruInput] =
    ObjectFieldsBinding.rmap {
      case List(
        SemesterBinding("semester", rSemester),
        SubaruCallForProposalsTypeBinding("subaruType", rSubaruType)
      ) => (rSemester, rSubaruType).parMapN(apply)
    }

}
