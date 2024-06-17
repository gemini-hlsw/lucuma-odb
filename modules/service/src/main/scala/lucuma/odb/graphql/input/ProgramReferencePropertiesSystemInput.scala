// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObjectFieldsBinding
import lucuma.odb.graphql.binding.DescriptionBinding
import lucuma.core.model.ProgramReference

case class ProgramReferencePropertiesSystemInput(
  description: ProgramReference.Description
)

object ProgramReferencePropertiesSystemInput {

  val Binding: Matcher[ProgramReferencePropertiesSystemInput] =
    ObjectFieldsBinding.rmap {
      case List(
        DescriptionBinding("description", rDescription)
      ) => rDescription.map(apply)
    }

}
