// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.either.*
import cats.syntax.parallel.*
import coulomb.Quantity
import coulomb.syntax.withUnit
import edu.gemini.grackle.Result
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.math.Wavelength
import lucuma.core.math.units.Angstrom
import lucuma.core.math.units.Micrometer
import lucuma.core.math.units.Nanometer
import lucuma.core.math.units.Picometer
import lucuma.core.optics.Format
import lucuma.odb.graphql.binding.*

import java.math.RoundingMode
import scala.util.control.Exception.*

object WavelengthDitherInput {

  val Picometers: Matcher[Quantity[Int, Picometer]] =
    IntBinding.map(_.withUnit[Picometer])

  private def matchToPm(name: String, scale: Int): Matcher[Quantity[Int, Picometer]] =
    BigDecimalBinding.emap { bd =>
      nonFatalCatch.either(
        bd.bigDecimal.movePointRight(scale).setScale(0, RoundingMode.HALF_UP).intValueExact()
      ).bimap(_ => s"Invalid wavelength dither in $name", _.withUnit[Picometer])
    }

  val Angstroms: Matcher[Quantity[Int, Picometer]] =
    matchToPm("angstrom", 2)

  val Nanometers: Matcher[Quantity[Int, Picometer]] =
    matchToPm("nanometer", 3)

  val Micrometers: Matcher[Quantity[Int, Picometer]] =
    matchToPm("micrometer", scale=6)

  val Binding: Matcher[Quantity[Int, Picometer]] =
    ObjectFieldsBinding.rmap {
      case List(
        Picometers.Option("picometers", rPicometers),
        Angstroms.Option("angstroms", rAngstroms),
        Nanometers.Option("nanometers", rNanometers),
        Micrometers.Option("micrometers", rMicrometers)
      ) =>
        (rPicometers, rAngstroms, rNanometers, rMicrometers).parTupled.flatMap {
          case (picometers, angstroms, nanometers, micrometers) =>
            oneOrFail(
              picometers  -> "picometers",
              angstroms   -> "angstroms",
              nanometers  -> "nanometers",
              micrometers -> "micrometers"
            )
        }
    }

}
