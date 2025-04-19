// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import grackle.Result
import lucuma.core.math.WavelengthDither
import lucuma.core.optics.Format
import lucuma.odb.graphql.binding.*

object WavelengthDitherInput {

  val Picometers: Matcher[WavelengthDither] =
    IntBinding.map(WavelengthDither.intPicometers.get)

  private def matchDecimal(name: String, f: Format[BigDecimal, WavelengthDither]): Matcher[WavelengthDither] =
    BigDecimalBinding.emap { bd =>
      f.getOption(bd).toRight(s"Invalid wavelength dither in $name")
    }

  val Angstroms: Matcher[WavelengthDither] =
    matchDecimal("angstrom", WavelengthDither.decimalAngstroms)

  val Nanometers: Matcher[WavelengthDither] =
    matchDecimal("nanometer", WavelengthDither.decimalNanometers)

  val Micrometers: Matcher[WavelengthDither] =
    matchDecimal("micrometer", WavelengthDither.decimalMicrometers)

  val Binding: Matcher[WavelengthDither] =
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
