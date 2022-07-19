package lucuma.odb.graphql.snippet
package input
package sourceprofile

import cats.syntax.all._
import edu.gemini.grackle.Result
import lucuma.core.enum.Band
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.BrightnessValue
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

  val BrightnessValueBinding: Matcher[BrightnessValue] =
    BigDecimalBinding.map(BrightnessValue.fromBigDecimal.get)

  def binding[A](
    unitsBinding: Matcher[Units Of Brightness[A]]
  ): Matcher[(Band, BrightnessMeasure[A])] =
    ObjectFieldsBinding.rmap {
      case List(
        BandBinding("band", rBand),
        BrightnessValueBinding.Option("value", rValue),
        unitsBinding.Option("units", rUnits),
        BrightnessValueBinding.Option("error", rError),
      ) => (rBand, rValue, rUnits, rError).parTupled.flatMap {
        case (band, Some(value), Some(units), error) =>
          Result((band, units.withValueTagged(value, error)))
        case _ =>
          Result.failure(s"Both value and units are required on creation.")
      }
    }

}