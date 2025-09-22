// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input
package sourceprofile

import cats.syntax.all.*
import grackle.Result
import lucuma.core.model.SourceProfile
import lucuma.odb.graphql.binding.*

object GaussianInput {

  val Binding: Matcher[SourceProfile.Gaussian] =
    ObjectFieldsBinding.rmap {
      case List(
            AngleInput.Binding.Option("fwhm", rFwhm),
            SpectralDefinitionInput.Integrated.Binding
              .Option("spectralDefinition", rSpectralDefinition)
          ) =>
        (rFwhm, rSpectralDefinition).parTupled.flatMap {
          case (Some(fwhm), Some(spectralDefinition)) =>
            Result(SourceProfile.Gaussian(fwhm, spectralDefinition))
          case _                                      =>
            Result.failure("Both fwhm and spectralDefinition must be provided on creation")
        }
    }
}
