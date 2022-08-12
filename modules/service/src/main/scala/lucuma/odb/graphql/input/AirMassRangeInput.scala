// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.parallel._
import edu.gemini.grackle.Result
import lucuma.core.model.ElevationRange.AirMass
import lucuma.core.model.ElevationRange.AirMass.DecimalValue
import lucuma.odb.graphql.binding._
import lucuma.odb.graphql.util.Bindings._

final case class AirMassRangeInput(
  min: Option[DecimalValue],
  max: Option[DecimalValue]
) {

  def create: Result[AirMass] =
    Result.fromOption[AirMass](
      (min, max)
        .tupled
        .flatMap(AirMass.fromOrderedDecimalValues.getOption),
      "Creating an air mass range requires specifying both min and max where min < max"
    )

  def edit: AirMass => Result[AirMass] = am => {
    val mn = min.getOrElse(am.min)
    val mx = max.getOrElse(am.max)
    Result.fromOption[AirMass](
      AirMass.fromOrderedDecimalValues.getOption((mn, mx)),
      s"Editing the air mass range as specified would create an invalid range: min ($mn) >= max ($mx)"
    )
  }

}

object AirMassRangeInput {

  val AirMassDecimalValue: Matcher[DecimalValue] =
    BigDecimalBinding.emap { bd =>
      DecimalValue.from(bd).leftMap(m => s"Invalid air mass constraint: $bd: $m")
    }

  val SimpleBinding: Matcher[AirMassRangeInput] =
    ObjectFieldsBinding.rmap {
      case List(
        AirMassDecimalValue.Option("min", rMin),
        AirMassDecimalValue.Option("max", rMax)
      ) => (rMin, rMax).parMapN(AirMassRangeInput(_, _))
    }

  val CreateBinding: Matcher[AirMass] =
    SimpleBinding.rmap(_.create)

  val EditBinding: Matcher[AirMass => Result[AirMass]] =
    SimpleBinding.map(_.edit)

}
