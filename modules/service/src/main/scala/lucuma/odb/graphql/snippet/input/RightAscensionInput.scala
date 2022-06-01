package lucuma.odb.graphql.snippet
package input

import lucuma.odb.graphql.util.Bindings._
import lucuma.core.math.RightAscension
import edu.gemini.grackle.Result
import cats.syntax.all._
import lucuma.odb.graphql.snippet.binding.HourAngleBinding
import lucuma.core.math.HourAngle

object RightAscensionInput {

  val Binding: Matcher[RightAscension] =
    ObjectFieldsBinding.rmap {
      case List(
        HourAngleBinding.Microarcseconds.Option("microarcseconds", rMicroarcseconds),
        HourAngleBinding.Degrees.Option("degrees", rDegrees),
        HourAngleBinding.Hours.Option("hours", rHours),
        HourAngleBinding.Hms.Option("hms", rHms),
        RightAscensionLongInput.Binding.Option("fromLong", rFromLong),
        RightAscensionDecimalInput.Binding.Option("fromDecimal", rFromDecimal),
      ) => (rMicroarcseconds, rDegrees, rHours, rHms, rFromLong, rFromDecimal).tupled.flatMap {
        case (microarcseconds, degrees, hours, hms, fromLong, fromDecimal) =>
          List(microarcseconds, degrees, hours, hms, fromLong, fromDecimal).flatten match {
            case List(ha) => Result(RightAscension(ha))
            case has => Result.failure(s"Expected exactly one right ascension format; found ${has.length}.")
          }
      }
    }
}

object RightAscensionLongInput {

  val Binding: Matcher[HourAngle] =
    LongInput("RightAscensionUnits") {
      case (value, "MICROARCSECONDS") => Result(HourAngle.fromMicroseconds(value))
      case (value, "DEGREES") => Result(HourAngle.fromDoubleDegrees(value.toDouble))
      case (value, "HOURS") => Result(HourAngle.fromDoubleHours(value.toDouble))
    }

}

object RightAscensionDecimalInput {

  val Binding: Matcher[HourAngle] =
    DecimalInput("RightAscensionUnits") {
      case (value, "MICROARCSECONDS") => Result(HourAngle.fromMicroseconds(value.toLong))
      case (value, "DEGREES") => Result(HourAngle.fromDoubleDegrees(value.toDouble))
      case (value, "HOURS") => Result(HourAngle.fromDoubleHours(value.toDouble))
    }

}

