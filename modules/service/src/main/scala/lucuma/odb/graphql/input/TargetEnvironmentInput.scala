// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import lucuma.core.model.Target
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.TargetIdBinding
import lucuma.odb.graphql.binding.*

//# Target environment editing and creation parameters
//input TargetEnvironmentInput {
//  # The explicitBase field may be unset by assigning a null value, or ignored by skipping it altogether
//  explicitBase: CoordinatesInput
//  asterism: [TargetId!]
//}

final case class TargetEnvironmentInput(
  explicitBase: Nullable[CoordinatesInput],
  asterism:     Nullable[List[Target.Id]]
)

object TargetEnvironmentInput {

  val Binding: Matcher[TargetEnvironmentInput] =
    ObjectFieldsBinding.rmap {
      case List(
        CoordinatesInput.Binding.Nullable("explicitBase", rBase),
        TargetIdBinding.List.Nullable("asterism", rAsterism)
      ) => (rBase, rAsterism).parMapN(TargetEnvironmentInput(_, _))
    }

}
