// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel._
import edu.gemini.grackle.Result
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.math.Wavelength
import lucuma.core.optics.Format
import lucuma.odb.graphql.binding._

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

  def oneOrFail(all: Option[Wavelength]*): Result[Wavelength] =
    all.toList.flatten match {
      case List(w) => Result(w)
      case _       => Result.failure("Expected exactly one of picometers, angstroms, nanometers, or micrometers.")
    }

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
            oneOrFail(picometers, angstroms, nanometers, micrometers)
        }
    }
}
