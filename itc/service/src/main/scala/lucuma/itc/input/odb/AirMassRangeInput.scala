// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.parallel.*
import eu.timepit.refined.api.Refined.value
import eu.timepit.refined.types.numeric.PosBigDecimal
import grackle.Result
import lucuma.core.model.AirMassBound
import lucuma.core.model.ElevationRange
import lucuma.odb.graphql.binding.*

final case class AirMassRangeInput(
  min: Option[AirMassBound],
  max: Option[AirMassBound]
) {

  def minPosBigDecimal: Option[PosBigDecimal] =
    min.flatMap(dv => PosBigDecimal.from(dv.value.value.value.value).toOption)

  def maxPosBigDecimal: Option[PosBigDecimal] =
    max.flatMap(dv => PosBigDecimal.from(dv.value.value.value.value).toOption)

  def create: Result[ElevationRange.ByAirMass] =
    Result.fromOption(
      (min, max).tupled
        .flatMap(ElevationRange.ByAirMass.FromOrderedBounds.getOption),
      AirMassRangeInput.messages.BothMinAndMax
    )

}

object AirMassRangeInput {

  object messages {
    val BothMinAndMax: String =
      "Creating an air mass range requires specifying both min and max where min < max"
  }

  val Default: AirMassRangeInput =
    AirMassRangeInput(
      ElevationRange.ByAirMass.DefaultMin.some,
      ElevationRange.ByAirMass.DefaultMax.some
    )

  val AirMassBoundValue: Matcher[AirMassBound] =
    BigDecimalBinding.emap { bd =>
      AirMassBound.fromBigDecimal(bd).leftMap(m => s"Invalid air mass constraint: $bd: $m")
    }

  val Binding: Matcher[AirMassRangeInput] =
    ObjectFieldsBinding.rmap {
      case List(
            AirMassBoundValue.Option("min", rMin),
            AirMassBoundValue.Option("max", rMax)
          ) =>
        (rMin, rMax).parMapN(AirMassRangeInput(_, _))
    }

}
