// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.option._
import cats.syntax.parallel._
import edu.gemini.grackle.Result
import eu.timepit.refined.api.Refined.value
import eu.timepit.refined.types.numeric.PosBigDecimal
import lucuma.core.model.ElevationRange.AirMass
import lucuma.core.model.ElevationRange.AirMass.DecimalValue
import lucuma.odb.graphql.binding._
import lucuma.odb.graphql.util.Bindings._

final case class AirMassRangeInput(
  min: Option[DecimalValue],
  max: Option[DecimalValue]
) {

  def minPosBigDecimal: Option[PosBigDecimal] =
    min.flatMap(dv => PosBigDecimal.from(dv.value).toOption)

  def maxPosBigDecimal: Option[PosBigDecimal] =
    max.flatMap(dv => PosBigDecimal.from(dv.value).toOption)

  def create: Result[AirMass] =
    Result.fromOption[AirMass](
      (min, max)
        .tupled
        .flatMap(AirMass.fromOrderedDecimalValues.getOption),
      "Creating an air mass range requires specifying both min and max where min < max"
    )

}

object AirMassRangeInput {

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
