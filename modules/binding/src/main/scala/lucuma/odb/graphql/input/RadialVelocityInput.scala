// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.data.OptionT
import cats.syntax.all.*
import grackle.Result
import lucuma.core.math.RadialVelocity
import lucuma.odb.graphql.binding.*

object RadialVelocityInput {

  val Binding: Matcher[RadialVelocity] =
    ObjectFieldsBinding.rmap {
      case List(
        LongBinding.Option("centimetersPerSecond", rCentimetersPerSecond),
        BigDecimalBinding.Option("metersPerSecond", rMetersPerSecond),
        BigDecimalBinding.Option("kilometersPerSecond", rKilometersPerSecond),
      ) =>
        val rCentimetersPerSecondʹ = OptionT(rCentimetersPerSecond).map(BigDecimal(_)).semiflatMap(resultFromCentimetersPerSecond).value
        val rMetersPerSecondʹ      = OptionT(rMetersPerSecond).semiflatMap(resultFromMetersPerSecond).value
        val rKilometersPerSecondʹ  = OptionT(rKilometersPerSecond).semiflatMap(resultFromKilometersPerSecond).value
        (rCentimetersPerSecondʹ, rMetersPerSecondʹ, rKilometersPerSecondʹ).parFlatMapN {
          (centimetersPerSecond, metersPerSecond, kilometersPerSecond) =>
            oneOrFail(
              centimetersPerSecond -> "centimetersPerSecond",
              metersPerSecond      -> "metersPerSecond",
              kilometersPerSecond  -> "kilometersPerSecond"
            )
        }
    }

  def resultFromCentimetersPerSecond(cmps: BigDecimal): Result[RadialVelocity] =
    resultFromMetersPerSecond(cmps / BigDecimal(100))

  def resultFromMetersPerSecond(mps: BigDecimal): Result[RadialVelocity] =
    Result.fromOption(RadialVelocity.fromMetersPerSecond.getOption(mps), s"Radial velocity cannot exceed the speed of light.")

  def resultFromKilometersPerSecond(kmps: BigDecimal): Result[RadialVelocity] =
    resultFromMetersPerSecond(kmps * BigDecimal(1000))

}
