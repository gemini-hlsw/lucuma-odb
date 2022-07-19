package lucuma.odb.graphql.snippet
package binding

import lucuma.odb.graphql.util.Bindings._
import lucuma.core.math.HourAngle

object HourAngleBinding {

  val Microarcseconds: Matcher[HourAngle] =
    LongBinding.map(HourAngle.fromMicroseconds)

  val Degrees: Matcher[HourAngle] =
    BigDecimalBinding.map(bd => HourAngle.fromDoubleDegrees(bd.toDouble))

  val Hours: Matcher[HourAngle] =
    BigDecimalBinding.map(bd => HourAngle.fromDoubleHours(bd.toDouble))

  val Hms: Matcher[HourAngle] =
    StringBinding.emap { s =>
      HourAngle.fromStringHMS.getOption(s).toRight(s"Invalid hour angle: $s")
    }

}
