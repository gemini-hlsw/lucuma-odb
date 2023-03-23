// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input
package sourceprofile

import cats.syntax.all._
import edu.gemini.grackle.Result
import lucuma.core.model.SourceProfile
import lucuma.odb.graphql.binding._

object GaussianInput {

  val CreateBinding: Matcher[SourceProfile.Gaussian] =
    ObjectFieldsBinding.rmap {
      case List(
        AngleInput.Binding.Option("fwhm", rFwhm),
        SpectralDefinitionInput.Integrated.CreateBinding.Option("spectralDefinition", rSpectralDefinition)
      ) =>
        (rFwhm, rSpectralDefinition).parTupled.flatMap {
          case (Some(fwhm), Some(spectralDefinition)) => Result(SourceProfile.Gaussian(fwhm, spectralDefinition))
          case _ => Result.failure("Both fwhm and spectralDefinition must be provided on creation")
        }
    }

  val EditBinding: Matcher[SourceProfile.Gaussian => Result[SourceProfile.Gaussian]] =
    ObjectFieldsBinding.rmap {
      case List(
        AngleInput.Binding.Option("fwhm", rFwhm),
        SpectralDefinitionInput.Integrated.EditBinding.Option("spectralDefinition", rSpectralDefinition)
      ) =>
        (rFwhm, rSpectralDefinition).parMapN {
          case (None, None)       => g => Result(g)
          case (Some(v), None)    => g => Result(g.copy(fwhm = v))
          case (None, Some(f))    => g => f(g.spectralDefinition).map(sd => g.copy(spectralDefinition = sd))
          case (Some(v), Some(f)) => g => f(g.spectralDefinition).map(sd => g.copy(fwhm = v, spectralDefinition = sd))
        }

    }

  val CreateOrEditBinding =
    CreateBinding or EditBinding

}
