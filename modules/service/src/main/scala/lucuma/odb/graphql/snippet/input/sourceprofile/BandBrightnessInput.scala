// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.snippet
package input
package sourceprofile

import cats.syntax.all._
import edu.gemini.grackle.Result
import lucuma.core.enums.Band
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.dimensional._
import lucuma.odb.graphql.util.Bindings._

object BandBrightnessInput {

  object Integrated {
    val Binding: Matcher[(Band, BrightnessMeasure[Integrated])] =
      binding(enumeratedBinding[Units Of Brightness[Integrated]])
  }

  object Surface {
    val Binding: Matcher[(Band, BrightnessMeasure[Surface])] =
      binding(enumeratedBinding[Units Of Brightness[Surface]])
  }

  def binding[A](
    unitsBinding: Matcher[Units Of Brightness[A]]
  ): Matcher[(Band, BrightnessMeasure[A])] =
    ObjectFieldsBinding.rmap {
      case List(
        BandBinding("band", rBand),
        BigDecimalBinding.Option("value", rValue),
        unitsBinding.Option("units", rUnits),
        BigDecimalBinding.Option("error", rError),
      ) => (rBand, rValue, rUnits, rError).parTupled.flatMap {
        case (band, Some(value), Some(units), error) =>
          Result((band, units.withValueTagged(value, error)))
        case _ =>
          Result.failure(s"Both value and units are required on creation.")
      }
    }

}