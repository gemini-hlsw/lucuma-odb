// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.snippet
package input

import cats.data.OptionT
import cats.syntax.all._
import edu.gemini.grackle.Result
import lucuma.core.math.RadialVelocity
import lucuma.odb.graphql.util.Bindings._

object RadialVelocityInput {

  val Binding: Matcher[RadialVelocity] =
    ObjectFieldsBinding.rmap {
      case List(
        LongBinding.Option("centimetersPerSecond", rCentimetersPerSecond),
        BigDecimalBinding.Option("metersPerSecond", rMetersPerSecond),
        BigDecimalBinding.Option("kilometersPerSecond", rKilometersPerSecond),
        RadialVelocityLongInput.Binding.Option("fromLong", rFromLong),
        RadialVelocityDecimalInput.Binding.Option("fromDecimal", rFromDecimal),
      ) =>
        val rCentimetersPerSecondʹ = OptionT(rCentimetersPerSecond).map(BigDecimal(_)).semiflatMap(resultFromCentimetersPerSecond).value
        val rMetersPerSecondʹ      = OptionT(rMetersPerSecond).semiflatMap(resultFromMetersPerSecond).value
        val rKilometersPerSecondʹ  = OptionT(rKilometersPerSecond).semiflatMap(resultFromKilometersPerSecond).value
        (rCentimetersPerSecondʹ, rMetersPerSecondʹ, rKilometersPerSecondʹ, rFromLong, rFromDecimal).parTupled.flatMap {
          case (centimetersPerSecond, metersPerSecond, kilometersPerSecond, fromLong, fromDecimal) =>
            List(centimetersPerSecond, metersPerSecond, kilometersPerSecond, fromLong, fromDecimal).flatten match {
              case List(r) => Result(r)
              case other   => Result.failure(s"Expected exactly one RadialVelocity representation; found ${other.length}.")
            }
        }
    }

  def resultFromCentimetersPerSecond(cmps: BigDecimal): Result[RadialVelocity] =
    resultFromMetersPerSecond(cmps / BigDecimal(100))

  def resultFromMetersPerSecond(mps: BigDecimal): Result[RadialVelocity] =
    Result.fromOption(RadialVelocity.fromMetersPerSecond.getOption(mps), s"Radial velocity cannot exceed the speed of light.")

  def resultFromKilometersPerSecond(kmps: BigDecimal): Result[RadialVelocity] =
    resultFromMetersPerSecond(kmps * BigDecimal(1000))

}

object RadialVelocityLongInput {
  import RadialVelocityInput._

  def Binding: Matcher[RadialVelocity] =
    LongInput("RadialVelocityUnits") {
      case (value, "CENTIMETERS_PER_SECOND") => resultFromCentimetersPerSecond(BigDecimal(value))
      case (value, "METERS_PER_SECOND")      => resultFromMetersPerSecond(BigDecimal(value))
      case (value, "KILOMETERS_PER_SECOND")  => resultFromKilometersPerSecond(BigDecimal(value))
    }

}

object RadialVelocityDecimalInput {
  import RadialVelocityInput._

  def Binding: Matcher[RadialVelocity] =
    DecimalInput("RadialVelocityUnits") {
      case (value, "CENTIMETERS_PER_SECOND") => resultFromCentimetersPerSecond(value)
      case (value, "METERS_PER_SECOND")      => resultFromMetersPerSecond(value)
      case (value, "KILOMETERS_PER_SECOND")  => resultFromKilometersPerSecond(value)
    }

}

