// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input
package sourceprofile

import cats.syntax.all.*
import grackle.Result
import lucuma.core.enums.Band
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.BrightnessValue
import lucuma.core.math.dimensional.*
import lucuma.core.util.*
import lucuma.odb.graphql.binding.*

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
            BrightnessValueBinding.Option("value", rValue),
            unitsBinding.Option("units", rUnits),
            BrightnessValueBinding.Option("error", rError)
          ) =>
        (rBand, rValue, rUnits, rError).parTupled.flatMap {
          case (band, Some(value), Some(units), error) =>
            Result((band, units.withValueTagged(value, error)))
          case _                                       =>
            Result.failure(s"Both value and units are required on creation.")
        }
    }

}
