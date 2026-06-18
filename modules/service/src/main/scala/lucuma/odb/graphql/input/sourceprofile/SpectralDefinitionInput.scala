// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input
package sourceprofile

import cats.data.Ior
import cats.syntax.all.*
import grackle.Result
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.SpectralDefinition.BandNormalized
import lucuma.core.model.SpectralDefinition.EmissionLines
import lucuma.odb.graphql.binding.*

object SpectralDefinitionInput {

  extension [A](self: SpectralDefinition[A])
    def bandNormalized = self match {
      case a: BandNormalized[A] => Result(a)
      case _ => Matcher.validationFailure("Not a band normalized spectral definition.")
    }

    def emissionLines  = self match {
      case a: EmissionLines[A]  => Result(a)
      case u => Matcher.validationFailure(s"Not an emission lines spectral definition ${u}.")
    }

    /** True if `self` and `other` are the same kind of spectral definition (both band
      * normalized, or both emission lines).
      */
    def matches(other: SpectralDefinition[A]): Boolean =
      (self, other) match {
        case (_: BandNormalized[A], _: BandNormalized[A]) => true
        case (_: EmissionLines[A], _: EmissionLines[A])   => true
        case _                                            => false
      }

    /** Apply the result of a `Create.or(Edit)` binding against this existing definition:
      * a create-only (`Ior.Left`) input replaces, an edit-only (`Ior.Right`) input edits,
      * and a both (`Ior.Both`) input edits when the SED types match but replaces when they
      * differ. This is what lets a user switch a definition's SED type (e.g. band normalized
      * <-> emission lines) in place rather than failing the edit.
      */
    def createOrEdit(
      ce: Ior[SpectralDefinition[A], SpectralDefinition[A] => Result[SpectralDefinition[A]]]
    ): Result[SpectralDefinition[A]] =
      ce match {
        case Ior.Left(c)    => Result(c)
        case Ior.Right(e)   => e(self)
        case Ior.Both(c, e) => if (self.matches(c)) e(self) else Result(c)
      }

  object Integrated {

    val CreateBinding: Matcher[SpectralDefinition[Integrated]] =
      createBinding(
        BandNormalizedInput.Integrated.CreateBinding,
        EmissionLinesInput.Integrated.CreateBinding,
      )

    val EditBinding: Matcher[SpectralDefinition[Integrated] => Result[SpectralDefinition[Integrated]]] =
      editBinding(
        BandNormalizedInput.Integrated.EditBinding,
        EmissionLinesInput.Integrated.EditBinding,
      )

    val CreateOrEditBinding =
      CreateBinding.or(EditBinding)

  }

  object Surface {

    val CreateBinding: Matcher[SpectralDefinition[Surface]] =
      createBinding(
        BandNormalizedInput.Surface.CreateBinding,
        EmissionLinesInput.Surface.CreateBinding,
      )

    val EditBinding: Matcher[SpectralDefinition[Surface] => Result[SpectralDefinition[Surface]]] =
      editBinding(
        BandNormalizedInput.Surface.EditBinding,
        EmissionLinesInput.Surface.EditBinding,
      )

    val CreateOrEditBinding =
      CreateBinding.or(EditBinding)

  }

  def createBinding[A](
    bandNormalized: Matcher[BandNormalized[A]],
    emissionLines: Matcher[EmissionLines[A]],
  ): Matcher[SpectralDefinition[A]] =
    ObjectFieldsBinding.rmap {
      case List(
        bandNormalized.Option("bandNormalized", rBandNormalized),
        emissionLines.Option("emissionLines", rEmissionLines),
      ) =>
        (rBandNormalized, rEmissionLines).parTupled.flatMap {
          case (Some(bandNormalized), None) => Result(bandNormalized)
          case (None, Some(emissionLines))  => Result(emissionLines)
          case _                            => Matcher.validationFailure("Expected exactly one of bandNormalized or emissionLines.")
        }
    }

  def editBinding[A](
    bandNormalized: Matcher[BandNormalized[A] => Result[BandNormalized[A]]],
    emissionLines: Matcher[EmissionLines[A] => EmissionLines[A]],
  ): Matcher[SpectralDefinition[A] => Result[SpectralDefinition[A]]] =
    ObjectFieldsBinding.rmap {
      case List(
        bandNormalized.Option("bandNormalized", rBandNormalized),
        emissionLines.Option("emissionLines", rEmissionLines),
      ) =>
        (rBandNormalized, rEmissionLines).parTupled.flatMap {
          case (Some(f), None) => Result(a => a.bandNormalized.flatMap(f))
          case (None, Some(f)) => Result(a => a.emissionLines.map(f))
          case _               => Matcher.validationFailure("Expected exactly one of bandNormalized or emissionLines.")
        }
    }

}














