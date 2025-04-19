// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import lucuma.core.enums.ScienceMode
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObjectFieldsBinding
import lucuma.odb.graphql.binding.enumeratedBinding

final case class ScienceRequirementsInput(
  mode:         Option[ScienceMode],
  spectroscopy: Option[SpectroscopyScienceRequirementsInput]
)

object ScienceRequirementsInput {

  val ModeBinding: Matcher[ScienceMode] =
    enumeratedBinding[ScienceMode]

  val Binding: Matcher[ScienceRequirementsInput] =
    ObjectFieldsBinding.rmap {
      case List(
        ModeBinding.Option("mode", rMode),
        SpectroscopyScienceRequirementsInput.Binding.Option("spectroscopy", rSpectroscopy)
      ) =>
        (rMode, rSpectroscopy).parMapN(apply)
    }

}
