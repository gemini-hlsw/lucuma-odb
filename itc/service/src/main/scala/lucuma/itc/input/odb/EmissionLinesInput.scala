// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input
package sourceprofile

import cats.kernel.Order
import cats.syntax.all.*
import grackle.Result
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.Wavelength
import lucuma.core.model.EmissionLine
import lucuma.core.model.SpectralDefinition.EmissionLines
import lucuma.odb.graphql.binding.*

import scala.collection.immutable.TreeMap

object EmissionLinesInput {

  implicit val WavelengthOrdering: Ordering[Wavelength] =
    Order[Wavelength].toOrdering

  object Integrated {
    val Binding: Matcher[EmissionLines[Integrated]] =
      binding[Integrated](
        EmissionLineInput.Integrated.CreateBinding,
        FluxDensityContinuumInput.Integrated.Binding
      )
  }

  object Surface {
    val Binding: Matcher[EmissionLines[Surface]] =
      binding[Surface](
        EmissionLineInput.Surface.CreateBinding,
        FluxDensityContinuumInput.Surface.Binding
      )
  }

  private def binding[A](
    line:                 Matcher[(Wavelength, EmissionLine[A])],
    fluxDensityContinuum: Matcher[FluxDensityContinuumMeasure[A]]
  ): Matcher[EmissionLines[A]] =
    ObjectFieldsBinding.rmap {
      case List(
            line.List.Option("lines", rLines),
            fluxDensityContinuum.Option("fluxDensityContinuum", rFluxDensityContinuum)
          ) =>
        (rLines, rFluxDensityContinuum).parTupled.flatMap {
          case (Some(lines), Some(fluxDensityContinuum)) =>
            Result(EmissionLines(lines.to(TreeMap), fluxDensityContinuum))
          case _                                         => Result.failure("Both lines and fluxDensityContinuum are required on creation.")
        }
    }
}
