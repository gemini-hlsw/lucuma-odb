// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input
package sourceprofile

import cats.kernel.Order
import cats.syntax.all._
import edu.gemini.grackle.Result
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.Wavelength
import lucuma.core.model.EmissionLine
import lucuma.core.model.SpectralDefinition.EmissionLines
import lucuma.odb.graphql.binding._

import scala.collection.immutable.TreeMap

object EmissionLinesInput {

  implicit val WavelengthOrdering: Ordering[Wavelength] =
    Order[Wavelength].toOrdering

  object Integrated {

    val CreateBinding: Matcher[EmissionLines[Integrated]] =
      createBinding[Integrated](
        EmissionLineInput.Integrated.CreateBinding,
        FluxDensityContinuumInput.Integrated.Binding,
      )

    val EditBinding: Matcher[EmissionLines[Integrated] => EmissionLines[Integrated]] =
      editBinding[Integrated](
        EmissionLineInput.Integrated.CreateBinding,
        FluxDensityContinuumInput.Integrated.Binding,
      )

  }

  object Surface {

    val CreateBinding: Matcher[EmissionLines[Surface]] =
      createBinding[Surface](
        EmissionLineInput.Surface.CreateBinding,
        FluxDensityContinuumInput.Surface.Binding,
      )

    val EditBinding: Matcher[EmissionLines[Surface] => EmissionLines[Surface]] =
      editBinding[Surface](
        EmissionLineInput.Surface.CreateBinding,
        FluxDensityContinuumInput.Surface.Binding,
      )

  }

  def createBinding[A](
    line: Matcher[(Wavelength, EmissionLine[A])],
    fluxDensityContinuum:  Matcher[FluxDensityContinuumMeasure[A]],
  ): Matcher[EmissionLines[A]] =
    ObjectFieldsBinding.rmap {
      case List(
        line.List.Option("lines", rLines),
        fluxDensityContinuum.Option("fluxDensityContinuum", rFluxDensityContinuum),
      ) =>
        (rLines, rFluxDensityContinuum).parTupled.flatMap {
          case (Some(lines), Some(fluxDensityContinuum)) => Result(EmissionLines(lines.to(TreeMap), fluxDensityContinuum))
          case _                                         => Result.failure("Both lines and fluxDensityContinuum are required on creation.")
        }
    }

  def editBinding[A](
    line: Matcher[(Wavelength, EmissionLine[A])],
    fluxDensityContinuum:  Matcher[FluxDensityContinuumMeasure[A]],
  ): Matcher[EmissionLines[A] => EmissionLines[A]] =
    ObjectFieldsBinding.rmap {
      case List(
        line.List.Option("lines", rLines),
        fluxDensityContinuum.Option("fluxDensityContinuum", rFluxDensityContinuum),
      ) =>
        (rLines, rFluxDensityContinuum).parMapN {
          (lines, fluxDensityContinuum) => in =>
            val a0 = lines.foldLeft(in)((in, ls) => in.copy(lines = ls.to(TreeMap)))
            val a1 = fluxDensityContinuum.foldLeft(a0)((in, fdc) => in.copy(fluxDensityContinuum = fdc))
            a1
        }
    }

}
