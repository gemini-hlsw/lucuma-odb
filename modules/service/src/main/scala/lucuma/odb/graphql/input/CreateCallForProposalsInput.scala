// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObjectFieldsBinding

case class CreateCallForProposalsInput(
  SET: CallForProposalsPropertiesInput.Create
)

object CreateCallForProposalsInput {

  val Binding: Matcher[CreateCallForProposalsInput] =
    ObjectFieldsBinding.rmap {
      case List(
        CallForProposalsPropertiesInput.Create.Binding("SET", rProps)
      ) => rProps.map(CreateCallForProposalsInput.apply)
    }

}
