package lucuma.odb.graphql.snippet
package input

import lucuma.odb.graphql.util.Bindings._
import lucuma.core.math.Declination
import edu.gemini.grackle.Result
import cats.syntax.all._
import lucuma.odb.graphql.snippet.binding.AngleBinding
import lucuma.core.math.Angle

object DeclinationInput {

  val Binding: Matcher[Declination] =
    ObjectFieldsBinding.rmap {
      case List(
        AngleBinding.Microarcseconds.Option("microarcseconds", rMicroarcseconds),
        AngleBinding.Degrees.Option("degrees", rDegrees),
        AngleBinding.Dms.Option("dms", rDms),
        DeclinationLongInput.Binding.Option("fromLong", rFromLong),
        DeclinationDecimalInput.Binding.Option("fromDecimal", rFromDecimal),
      ) => (rMicroarcseconds, rDegrees, rDms, rFromLong, rFromDecimal).parTupled.flatMap {
        case (microarcseconds, degrees, hms, fromLong, fromDecimal) =>
          List(microarcseconds, degrees, hms, fromLong, fromDecimal).flatten match {
            case List(a) =>
              Result.fromOption(Declination.fromAngle.getOption(a), s"Invalid declination: ${Angle.fromStringDMS.reverseGet(a)}")
            case as => Result.failure(s"Expected exactly one declination format; found ${as.length}.")
          }
      }
    }

}

object DeclinationLongInput {

  val Binding: Matcher[Angle] =
    LongInput("DeclinationUnits") {
      case (value, "MICROARCSECONDS") => Result(Angle.fromMicroarcseconds(value))
      case (value, "DEGREES") => Result(Angle.fromDoubleDegrees(value.toDouble))
    }

  }

object DeclinationDecimalInput {

  val Binding: Matcher[Angle] =
    DecimalInput("DeclinationUnits") {
      case (value, "MICROARCSECONDS") => Result(Angle.fromMicroarcseconds(value.toLong))
      case (value, "DEGREES") => Result(Angle.fromDoubleDegrees(value.toDouble))
    }

}

