// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input.sourceprofile

import cats.syntax.all.*
import grackle.Result
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.Wavelength
import lucuma.core.model.EmissionLine
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.*

object EmissionLineInput {

  object Integrated {
    val CreateBinding = createBinding[Integrated](LineFluxInput.Integrated.Binding)
    val EditBinding   = editBinding[Integrated](LineFluxInput.Integrated.Binding)
  }

  object Surface {
    val CreateBinding = createBinding[Surface](LineFluxInput.Surface.Binding)
    val EditBinding   = editBinding[Surface](LineFluxInput.Surface.Binding)
  }

  def createBinding[A](
    lineFlux: Matcher[LineFluxMeasure[A]]
  ): Matcher[(Wavelength, EmissionLine[A])] =
    ObjectFieldsBinding.rmap {
      case List(
            WavelengthInput.Binding("wavelength", rWavelength),
            LineWidthBinding.Option("lineWidth", rLineWidth),
            lineFlux.Option("lineFlux", rLineFlux)
          ) =>
        (rWavelength, rLineWidth, rLineFlux).parTupled.flatMap {
          case (wavelength, Some(lineWidth), Some(lineFlux)) =>
            Result((wavelength, EmissionLine(lineWidth, lineFlux)))
          case _                                             => Result.failure("All fields are required on creation.")
        }
    }

  def editBinding[A](
    lineFlux: Matcher[LineFluxMeasure[A]]
  ): Matcher[(Wavelength, EmissionLine[A] => EmissionLine[A])] =
    ObjectFieldsBinding.rmap {
      case List(
            WavelengthInput.Binding("wavelength", rWavelength),
            LineWidthBinding.Option("lineWidth", rLineWidth),
            lineFlux.Option("lineFlux", rLineFlux)
          ) =>
        (rWavelength, rLineWidth, rLineFlux).parMapN { (wavelength, lineWidth, lineFlux) =>
          val edit = (in: EmissionLine[A]) => {
            val a0 = lineWidth.foldLeft(in)((el, lw) => el.copy(lineWidth = lw))
            val a1 = lineFlux.foldLeft(a0)((el, lf) => el.copy(lineFlux = lf))
            a1
          }
          (wavelength, edit)
        }
    }

}
