// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input
package sourceprofile

import cats.syntax.all.*
import grackle.Result
import lucuma.core.math.Wavelength
import lucuma.core.optics.Format
import lucuma.odb.graphql.binding.*

object WavelengthInput {

  def getResult[A, B](f: Format[A, B], name: String): A => Result[B] = a =>
    Result.fromOption(f.getOption(a), s"Invalid $name value: $a")

  val getPicometers:  BigDecimal => Result[Wavelength] = getResult(Wavelength.decimalPicometers, "picometers")
  val getAngstroms:   BigDecimal => Result[Wavelength] = getResult(Wavelength.decimalAngstroms, "angstroms")
  val getNanometers:  BigDecimal => Result[Wavelength] = getResult(Wavelength.decimalNanometers, "nanometers")
  val getMicrometers: BigDecimal => Result[Wavelength] = getResult(Wavelength.decimalMicrometers, "micrometers")

  val Picometers  = LongBinding.map(BigDecimal(_)).rmap(PartialFunction.fromFunction(getPicometers))
  val Angstroms   = BigDecimalBinding.rmap(PartialFunction.fromFunction(getAngstroms))
  val Nanometers  = BigDecimalBinding.rmap(PartialFunction.fromFunction(getNanometers))
  val Micrometers = BigDecimalBinding.rmap(PartialFunction.fromFunction(getMicrometers))


  val Binding: Matcher[Wavelength] =
    ObjectFieldsBinding.rmap {
      case List(
        Picometers.Option("picometers", rPicometers),
        Angstroms.Option("angstroms", rAngstroms),
        Nanometers.Option("nanometers", rNanometers),
        Micrometers.Option("micrometers", rMicrometers),
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