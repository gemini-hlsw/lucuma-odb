package lucuma.odb.graphql
package snippet
package input

import lucuma.odb.graphql.util.Bindings._
import edu.gemini.grackle.Result
import cats.syntax.all._
import java.time.Duration

object DurationInput {

  def getMicroseconds(d: Long)       = Duration.ofNanos(d * 1000L)
  def getMilliseconds(d: BigDecimal) = Duration.ofMillis(d.toLong)
  def getSeconds(d: BigDecimal)      = Duration.ofMillis((d * BigDecimal(1000)).toLong)
  def getMinutes(d: BigDecimal)      = Duration.ofMillis((d * BigDecimal(1000 * 60)).toLong)
  def getHours(d: BigDecimal)        = Duration.ofMillis((d * BigDecimal(1000 * 60 * 60)).toLong)

  // N.B. many of these truncate precision because there aren't decimal/double constructors. Need to go back and fix.
  val Microseconds    = LongBinding.map(getMicroseconds)
  val Milliseconds    = BigDecimalBinding.map(getMilliseconds)
  val Seconds         = BigDecimalBinding.map(getSeconds)
  val Minutes         = BigDecimalBinding.map(getMinutes)
  val Hours           = BigDecimalBinding.map(getHours)

  def oneOrFail(all: Option[Duration]*): Result[Duration] =
    all.toList.flatten match {
      case List(w) => Result(w)
      case _       => Result.failure("Expected exactly one of microseconds, milliarcseconds, milliseconds, arcseconds, seconds, arcminutes, minutes, degrees, hours, dms, hms, fromLong, fromDecimal.")
    }

  val Binding: Matcher[Duration] =
    ObjectFieldsBinding.rmap {
      case List(
        Microseconds.Option("microseconds", rMicroseconds),
        Milliseconds.Option("milliseconds", rMilliseconds),
        Seconds.Option("seconds", rSeconds),
        Minutes.Option("minutes", rMinutes),
        Hours.Option("hours", rHours),
      ) =>
        (rMicroseconds, rMilliseconds, rSeconds, rMinutes, rHours).parTupled.flatMap {
          case (microseconds, milliseconds, seconds, minutes, hours) =>
            oneOrFail(microseconds, milliseconds, seconds, minutes, hours)
        }
    }
}

