// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import grackle.Result
import lucuma.core.math.Wavelength
import lucuma.core.optics.Format
import lucuma.odb.graphql.binding.*

object WavelengthInput {

  private def fromFormat(f: Format[BigDecimal, Wavelength], n: String): Matcher[Wavelength] =
    PosBigDecimalBinding.emap { pbd =>
      f.getOption(pbd.value).toRight(s"Invalid Wavelength in $n: $pbd")
    }

  val Picometers: Matcher[Wavelength] =
    PosIntBinding.map(Wavelength.picometers.get)

  val Angstroms: Matcher[Wavelength] =
    fromFormat(Wavelength.decimalAngstroms, "angstroms")

  val Nanometers: Matcher[Wavelength] =
    fromFormat(Wavelength.decimalNanometers, "nanometers")

  val Micrometers: Matcher[Wavelength] =
    fromFormat(Wavelength.decimalMicrometers, "micrometers")

  val Binding: Matcher[Wavelength] =
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
