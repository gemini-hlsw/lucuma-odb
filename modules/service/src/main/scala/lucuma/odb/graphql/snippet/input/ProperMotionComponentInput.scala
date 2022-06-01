package lucuma.odb.graphql.snippet
package input

import lucuma.odb.graphql.util.Bindings._
import lucuma.core.math.ProperMotion
import lucuma.core.math.units._
import cats.data.Nested
import cats.syntax.all._
import coulomb._
import edu.gemini.grackle.Result
import lucuma.core.math.VelocityAxis

object ProperMotionComponentInput {

  object RA {
    val Binding = binding[VelocityAxis.RA]
  }

  object Dec {
    val Binding = binding[VelocityAxis.Dec]
  }

  private def binding[A]: Matcher[ProperMotion.AngularVelocityComponent[A]] =
    ObjectFieldsBinding.rmap {
      case List(
        LongBinding.Option("microarcsecondsPerYear", rMicroarcsecondsPerYear),
        BigDecimalBinding.Option("milliarcsecondsPerYear", rMilliarcsecondsPerYear),
        ProperMotionComponentLongInput.Binding.Option("fromLong", rFromLong),
        ProperMotionComponentDecimalInput.Binding.Option("fromBigDecimal", rFromBigDecimal),
      ) =>
        val rMicroarcsecondsPerYearʹ = Nested(rMicroarcsecondsPerYear).map(_.withUnit[MicroArcSecondPerYear]).value
        val rMilliarcsecondsPerYearʹ = Nested(rMilliarcsecondsPerYear).map(n => (n * 1000).toLong.withUnit[MicroArcSecondPerYear]).value
        (rMicroarcsecondsPerYearʹ, rMilliarcsecondsPerYearʹ, rFromLong, rFromBigDecimal).tupled.flatMap {
          case (microarcsecondsPerYear, milliarcsecondsPerYear, fomLong, fromBigDecimal) =>
            List(microarcsecondsPerYear, milliarcsecondsPerYear, fomLong, fromBigDecimal).flatten match {
              case List(a) => Result(ProperMotion.AngularVelocityComponent(a))
              case as => Result.failure(s"Expected exactly one proper motion component format; found ${as.length}.")
            }
        }
    }

}

object ProperMotionComponentLongInput {

  def Binding[A]: Matcher[Quantity[Long, MicroArcSecondPerYear]] =
    LongInput("ProperMotionComponentUnits") {
      case (value, "MICROARCSECONDS_PER_YEAR") => Result(value.withUnit)
      case (value, "MILLIARCSECONDS_PER_YEAR") => Result((value * 1000L).withUnit)
    }

}

object ProperMotionComponentDecimalInput {

  def Binding[A]: Matcher[Quantity[Long, MicroArcSecondPerYear]] =
    DecimalInput("ProperMotionComponentUnits") {
      case (value, "MICROARCSECONDS_PER_YEAR") => Result(value.toLong.withUnit)
      case (value, "MILLIARCSECONDS_PER_YEAR") => Result((value.toLong * 1000L).withUnit)
    }

}

