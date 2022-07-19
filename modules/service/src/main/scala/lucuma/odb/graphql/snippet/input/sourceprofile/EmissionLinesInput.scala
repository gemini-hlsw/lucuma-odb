// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.snippet
package input
package sourceprofile

import cats.kernel.Order
import cats.syntax.all._
import edu.gemini.grackle.Result
import eu.timepit.refined.types.numeric.PosBigDecimal
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.Wavelength
import lucuma.core.math.dimensional.Measure
import lucuma.core.math.dimensional.Of
import lucuma.core.model.EmissionLine
import lucuma.core.model.SpectralDefinition.EmissionLines
import lucuma.odb.graphql.util.Bindings._

import scala.collection.immutable.SortedMap
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
        EmissionLineInput.Integrated.EditBinding,
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
        EmissionLineInput.Surface.EditBinding,
        FluxDensityContinuumInput.Surface.Binding,
      )

  }

  def createBinding[A](
    line: Matcher[(Wavelength, EmissionLine[A])],
    fluxDensityContinuum:  Matcher[Measure[PosBigDecimal] Of FluxDensityContinuum[A]],
  ): Matcher[EmissionLines[A]] =
    ObjectFieldsBinding.rmap {
      case List(
        line.List.Option("lines", rLines),
        line.List.Option("editLines", rEditLines),
        WavelengthInput.Binding.List.Option("deleteLines", rDeleteLines),
        fluxDensityContinuum.Option("fluxDensityContinuum", rFluxDensityContinuum),
      ) =>
        (rLines, rEditLines, rDeleteLines, rFluxDensityContinuum).parTupled.flatMap {
          case (Some(lines), None, None, Some(fluxDensityContinuum)) => Result(EmissionLines(lines.to(TreeMap), fluxDensityContinuum))
          case (Some(lines), _, _, Some(fluxDensityContinuum))       => Result.warning("editLines and deleteLines are ignored on creation.", EmissionLines(lines.to(TreeMap), fluxDensityContinuum))
          case _                                                     => Result.failure("Both lines and fluxDensityContinuum are required on creation.")
        }
    }

  // y u not in stdlib?
  private def edit[A, B](m: SortedMap[A, B], k: A, f: B => B): SortedMap[A, B] =
    m.get(k) match {
      case None => m
      case Some(v) => m.updated(k, f(v))
    }

  def editBinding[A](
    line: Matcher[(Wavelength, EmissionLine[A])],
    editLine: Matcher[(Wavelength, EmissionLine[A] => EmissionLine[A])],
    fluxDensityContinuum:  Matcher[Measure[PosBigDecimal] Of FluxDensityContinuum[A]],
  ): Matcher[EmissionLines[A] => EmissionLines[A]] =
    ObjectFieldsBinding.rmap {
      case List(
        line.List.Option("lines", rLines),
        editLine.List.Option("editLines", rEditLines),
        WavelengthInput.Binding.List.Option("deleteLines", rDeleteLines),
        fluxDensityContinuum.Option("fluxDensityContinuum", rFluxDensityContinuum),
      ) =>
        (rLines, rEditLines, rDeleteLines, rFluxDensityContinuum).parMapN {
          (lines, editLines, deleteLines, fluxDensityContinuum) => in =>
            val a0 = lines.foldLeft(in)((in, ls) => in.copy(lines = ls.to(TreeMap)))
            val a1 = editLines.foldLeft(a0)((in, es) => in.copy(lines = es.foldLeft(in.lines)((m, e) => edit(m, e._1, e._2))))
            val a2 = deleteLines.foldLeft(a1)((in, ks) => in.copy(lines = in.lines -- ks))
            val a3 = fluxDensityContinuum.foldLeft(a2)((in, fdc) => in.copy(fluxDensityContinuum = fdc))
            a3
        }
    }

}