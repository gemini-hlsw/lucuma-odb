// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
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
            BigDecimalBinding.Option("kilometersPerSecond", rKilometersPerSecond)
          ) =>
        val rCentimetersPerSecondʹ = OptionT(rCentimetersPerSecond)
          .map(BigDecimal(_))
          .semiflatMap(resultFromCentimetersPerSecond)
          .value
        val rMetersPerSecondʹ      =
          OptionT(rMetersPerSecond).semiflatMap(resultFromMetersPerSecond).value
        val rKilometersPerSecondʹ  =
          OptionT(rKilometersPerSecond)
            .semiflatMap(resultFromKilometersPerSecond)
            .value

        // format: off
        // prevent code formatter from merging with the previous definition
        (rCentimetersPerSecondʹ, rMetersPerSecondʹ, rKilometersPerSecondʹ).parTupled
          .flatMap { case (centimetersPerSecond, metersPerSecond, kilometersPerSecond) =>
            List(centimetersPerSecond, metersPerSecond, kilometersPerSecond).flatten match {
              case List(r) => Result(r)
              case other   =>
                Result.failure(
                  s"Expected exactly one RadialVelocity representation; found ${other.length}."
                )
            }
          }
        // format: on
    }

  def resultFromCentimetersPerSecond(cmps: BigDecimal): Result[RadialVelocity] =
    resultFromMetersPerSecond(cmps / BigDecimal(100))

  def resultFromMetersPerSecond(mps: BigDecimal): Result[RadialVelocity] =
    Result.fromOption(RadialVelocity.fromMetersPerSecond.getOption(mps),
                      s"Radial velocity cannot exceed the speed of light."
    )

  def resultFromKilometersPerSecond(kmps: BigDecimal): Result[RadialVelocity] =
    resultFromMetersPerSecond(kmps * BigDecimal(1000))

}
