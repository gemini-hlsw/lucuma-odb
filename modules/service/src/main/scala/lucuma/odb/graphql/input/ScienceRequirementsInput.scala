// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.option._
import cats.syntax.parallel._
import lucuma.core.enums.ScienceMode
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObjectFieldsBinding
import lucuma.odb.graphql.binding.enumeratedBinding

/*
# Edit science requirements
input ScienceRequirementsInput {
  # The mode field must be either specified or skipped altogether.  It cannot be unset with a null value.
  mode: ScienceMode

  # The spectroscopy field must be either specified or skipped altogether.  It cannot be unset with a null value.
  spectroscopy: SpectroscopyScienceRequirementsInput
}
*/

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

//  val CreateBinding: Matcher[ScienceRequirementsInput] =
//    Binding.map { sri =>
//      ScienceRequirementsInput(
//        sri.mode.orElse(ScienceMode.Spectroscopy.some),
//        sri.spectroscopy.orElse(SpectroscopyScienceRequirementsInput.Default.some)
//      )
//    }

}