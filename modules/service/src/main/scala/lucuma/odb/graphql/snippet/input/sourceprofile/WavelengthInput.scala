// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.snippet
package input
package sourceprofile

import lucuma.core.math.Wavelength
import lucuma.odb.graphql.util.Bindings._
import edu.gemini.grackle.Result
import lucuma.core.optics.Format
import cats.syntax.all._

object WavelengthInput {

  def getResult[A, B](f: Format[A, B], name: String): A => Result[B] = a =>
    Result.fromOption(f.getOption(a), s"Invalid $name value: $a")

  val getPicometers:  BigDecimal => Result[Wavelength] = getResult(Wavelength.decimalPicometers, "picometers")
  val getAngstroms:   BigDecimal => Result[Wavelength] = getResult(Wavelength.decimalAngstroms, "angstroms")
  val getNanometers:  BigDecimal => Result[Wavelength] = getResult(Wavelength.decimalNanometers, "nanometers")
  val getMicrometers: BigDecimal => Result[Wavelength] = getResult(Wavelength.decimalMicrometers, "micrometers")

  def decimalInputHandler: PartialFunction[(BigDecimal, String), Result[Wavelength]] = {
    case (v, "PICOMETERS")  => getPicometers(v)
    case (v, "ANGSTROMS")   => getAngstroms(v)
    case (v, "NANOMETERS")  => getNanometers(v)
    case (v, "MICROMETERS") => getMicrometers(v)
  }

  val Picometers  = LongBinding.map(BigDecimal(_)).rmap(PartialFunction.fromFunction(getPicometers))
  val Angstroms   = BigDecimalBinding.rmap(PartialFunction.fromFunction(getAngstroms))
  val Nanometers  = BigDecimalBinding.rmap(PartialFunction.fromFunction(getNanometers))
  val Micrometers = BigDecimalBinding.rmap(PartialFunction.fromFunction(getMicrometers))
  val FromLong    = LongInput("Wavelength")(decimalInputHandler compose { case (v, s) => (BigDecimal(v), s) })
  val FromDecimal = DecimalInput("Wavelength")(decimalInputHandler)

  def oneOrFail(all: Option[Wavelength]*): Result[Wavelength] =
    all.toList.flatten match {
      case List(w) => Result(w)
      case _       => Result.failure("Expected exactly one of picometers, angstroms, nanometers, micrometers, fromLong, fromDecimal.")
    }

  val Binding: Matcher[Wavelength] =
    ObjectFieldsBinding.rmap {
      case List(
        Picometers.Option("picometers", rPicometers),
        Angstroms.Option("angstroms", rAngstroms),
        Nanometers.Option("nanometers", rNanometers),
        Micrometers.Option("micrometers", rMicrometers),
        FromLong.Option("fromLong", rFromLong),
        FromDecimal.Option("fromDecimal", rFromDecimal),
      ) =>
        (rPicometers, rAngstroms, rNanometers, rMicrometers, rFromLong, rFromDecimal).parTupled.flatMap {
          case (picometers, angstroms, nanometers, micrometers, fromLong, fromDecimal) =>
            oneOrFail(picometers, angstroms, nanometers, micrometers, fromLong, fromDecimal)
        }
    }
}