// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input
package sourceprofile

import cats.syntax.all.*
import grackle.Result
import lucuma.core.model.SourceProfile
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.sourceprofile.SpectralDefinitionInput.createOrEdit

object GaussianInput {

  val CreateBinding: Matcher[SourceProfile.Gaussian] =
    ObjectFieldsBinding.rmap {
      case List(
        AngleInput.Binding.Option("fwhm", rFwhm),
        SpectralDefinitionInput.Integrated.CreateBinding.Option("spectralDefinition", rSpectralDefinition)
      ) =>
        (rFwhm, rSpectralDefinition).parTupled.flatMap {
          case (Some(fwhm), Some(spectralDefinition)) => Result(SourceProfile.Gaussian(fwhm, spectralDefinition))
          case _ => Matcher.validationFailure("Both fwhm and spectralDefinition must be provided on creation")
        }
    }

  val EditBinding: Matcher[SourceProfile.Gaussian => Result[SourceProfile.Gaussian]] =
    ObjectFieldsBinding.rmap {
      case List(
        AngleInput.Binding.Option("fwhm", rFwhm),
        SpectralDefinitionInput.Integrated.CreateOrEditBinding.Option("spectralDefinition", rSpectralDefinition)
      ) =>
        (rFwhm, rSpectralDefinition).parMapN { (fwhmOpt, sdOpt) => g =>
          val fwhm = fwhmOpt.getOrElse(g.fwhm)
          // Edit the spectral definition in place when the SED type matches, otherwise replace it.
          sdOpt
            .fold(Result(g.spectralDefinition))(g.spectralDefinition.createOrEdit)
            .map(sd => g.copy(fwhm = fwhm, spectralDefinition = sd))
        }

    }

  val CreateOrEditBinding =
    CreateBinding.or(EditBinding)

}
