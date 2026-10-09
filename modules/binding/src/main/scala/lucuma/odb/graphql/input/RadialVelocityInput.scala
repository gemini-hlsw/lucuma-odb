// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import grackle.Result
import lucuma.core.math.RadialVelocity
import lucuma.odb.graphql.binding.*

object RadialVelocityInput {

  val Binding: Matcher[RadialVelocity] =
    OneOfBinding(
      "centimetersPerSecond" -> LongBinding.rmap(cmps => resultFromCentimetersPerSecond(BigDecimal(cmps))),
      "metersPerSecond"      -> BigDecimalBinding.rmap(resultFromMetersPerSecond(_)),
      "kilometersPerSecond"  -> BigDecimalBinding.rmap(resultFromKilometersPerSecond(_))
    )

  def resultFromCentimetersPerSecond(cmps: BigDecimal): Result[RadialVelocity] =
    resultFromMetersPerSecond(cmps / BigDecimal(100))

  def resultFromMetersPerSecond(mps: BigDecimal): Result[RadialVelocity] =
    Result.fromOption(RadialVelocity.fromMetersPerSecond.getOption(mps), s"Radial velocity cannot exceed the speed of light.")

  def resultFromKilometersPerSecond(kmps: BigDecimal): Result[RadialVelocity] =
    resultFromMetersPerSecond(kmps * BigDecimal(1000))

}
