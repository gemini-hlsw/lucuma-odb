// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.parallel.*
import edu.gemini.grackle.Result
import eu.timepit.refined.api.Refined.value
import eu.timepit.refined.types.numeric.PosBigDecimal
import lucuma.core.model.ElevationRange.AirMass
import lucuma.core.model.ElevationRange.AirMass.DecimalValue
import lucuma.odb.graphql.binding.*

final case class AirMassRangeInput(
  min: Option[DecimalValue],
  max: Option[DecimalValue]
) {

  def minPosBigDecimal: Option[PosBigDecimal] =
    min.flatMap(dv => PosBigDecimal.from(dv.value).toOption)

  def maxPosBigDecimal: Option[PosBigDecimal] =
    max.flatMap(dv => PosBigDecimal.from(dv.value).toOption)

  def create: Result[AirMass] =
    Result.fromOption(
      (min, max)
        .tupled
        .flatMap(AirMass.fromOrderedDecimalValues.getOption),
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
      AirMass.DefaultMin.some,
      AirMass.DefaultMax.some
    )

  val AirMassDecimalValue: Matcher[DecimalValue] =
    BigDecimalBinding.emap { bd =>
      DecimalValue.from(bd).leftMap(m => s"Invalid air mass constraint: $bd: $m")
    }

  val Binding: Matcher[AirMassRangeInput] =
    ObjectFieldsBinding.rmap {
      case List(
        AirMassDecimalValue.Option("min", rMin),
        AirMassDecimalValue.Option("max", rMax)
      ) => (rMin, rMax).parMapN(AirMassRangeInput(_, _))
    }

}
