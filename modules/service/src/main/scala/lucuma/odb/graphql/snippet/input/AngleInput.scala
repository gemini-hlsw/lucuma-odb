// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package snippet
package input

import cats.syntax.all._
import edu.gemini.grackle.Result
import lucuma.core.math.Angle
import lucuma.core.math.HourAngle
import lucuma.odb.graphql.util.Bindings._

object AngleInput {

  def getMicroarcseconds(d: Long)  = Angle.microarcseconds.reverseGet(d)
  def getMicroseconds(d: Long)     = HourAngle.fromMicroseconds(d)
  def getMilliarcseconds(d: Int)   = Angle.milliarcseconds.reverseGet(d)
  def getMilliseconds(d: Int)      = HourAngle.milliseconds.reverseGet(d)
  def getArcSeconds(d: Double)     = Angle.fromDoubleArcseconds(d)
  def getSeconds(d: Int)           = HourAngle.seconds.reverseGet(d)
  def getArcMinutes(d: Int)        = Angle.arcminutes.reverseGet(d)
  def getMinutes(d: Int)           = HourAngle.minutes.reverseGet(d)
  def getDegrees(d: Double)        = Angle.fromDoubleDegrees(d)
  def getHours(d: Double)          = HourAngle.fromDoubleHours(d)
  def getDMS(s: String)            = Angle.fromStringDMS.getOption(s).toRight(s"Invalid DMS angle: $s")
  def getHMS(s: String)            = HourAngle.fromStringHMS.getOption(s).toRight(s"Invalid HMS angle: $s")

  // N.B. many of these truncate precision because there aren't decimal/double constructors. Need to go back and fix.
  val Microarcseconds = LongBinding.map(getMicroarcseconds)
  val Microseconds    = BigDecimalBinding.map(_.toLong).map(getMicroseconds)
  val Milliarcseconds = BigDecimalBinding.map(_.toInt).map(getMilliarcseconds)
  val Milliseconds    = BigDecimalBinding.map(_.toInt).map(getMilliseconds)
  val ArcSeconds      = BigDecimalBinding.map(_.toDouble).map(getArcSeconds)
  val Seconds         = BigDecimalBinding.map(_.toInt).map(getSeconds)
  val ArcMinutes      = BigDecimalBinding.map(_.toInt).map(getArcMinutes)
  val Minutes         = BigDecimalBinding.map(_.toInt).map(getMinutes)
  val Degrees         = BigDecimalBinding.map(_.toDouble).map(getDegrees)
  val Hours           = BigDecimalBinding.map(_.toDouble).map(getHours)
  val DMS             = StringBinding.emap(getDMS)
  val HMS             = StringBinding.emap(getHMS)
  val FromLong        = LongInput("Angle")(decimalInputHandler compose { case (v, s) => (BigDecimal(v), s) })
  val FromDecimal     = DecimalInput("Angle")(decimalInputHandler)


  def decimalInputHandler: PartialFunction[(BigDecimal, String), Result[Angle]] = {
    case (v, "MICROARCSECONDS")    => Result(getMicroarcseconds(v.toLong))
    case (v, "MICROSECONDS")    => Result(getMicroseconds(v.toLong))
    case (v, "MILLIARCSECONDS") => Result(getMilliarcseconds(v.toInt))
    case (v, "MILLISECONDS")    => Result(getMilliseconds(v.toInt))
    case (v, "ARCSECONDS")      => Result(getArcSeconds(v.toDouble))
    case (v, "SECONDS")         => Result(getSeconds(v.toInt))
    case (v, "ARCMINUTES")      => Result(getArcMinutes(v.toInt))
    case (v, "MINUTES")         => Result(getMinutes(v.toInt))
    case (v, "DEGREES")         => Result(getDegrees(v.toDouble))
    case (v, "HOURS")           => Result(getHours(v.toDouble))
  }

  def oneOrFail(all: Option[Angle]*): Result[Angle] =
    all.toList.flatten match {
      case List(w) => Result(w)
      case _       => Result.failure("Expected exactly one of microarcseconds, microseconds, milliarcseconds, milliseconds, arcseconds, seconds, arcminutes, minutes, degrees, hours, dms, hms, fromLong, fromDecimal.")
    }

  val Binding: Matcher[Angle] =
    ObjectFieldsBinding.rmap {
      case List(
        Microarcseconds.Option("microarcseconds", rMicroarcseconds),
        Microseconds.Option("microseconds", rMicroseconds),
        Milliarcseconds.Option("milliarcseconds", rMilliarcseconds),
        Milliseconds.Option("milliseconds", rMilliseconds),
        ArcSeconds.Option("arcseconds", rArcSeconds),
        Seconds.Option("seconds", rSeconds),
        ArcMinutes.Option("arcminutes", rArcMinutes),
        Minutes.Option("minutes", rMinutes),
        Degrees.Option("degrees", rDegrees),
        Hours.Option("hours", rHours),
        DMS.Option("dms", rDMS),
        HMS.Option("hms", rHMS),
        FromLong.Option("fromlong", rFromLong),
        FromDecimal.Option("fromdecimal", rFromDecimal),
      ) =>
        (rMicroarcseconds, rMicroseconds, rMilliarcseconds, rMilliseconds, rArcSeconds, rSeconds, rArcMinutes, rMinutes, rDegrees, rHours, rDMS, rHMS, rFromLong, rFromDecimal).parTupled.flatMap {
          case (microarcseconds, microseconds, milliarcseconds, milliseconds, arcseconds, seconds, arcminutes, minutes, degrees, hours, dms, hms, fromLong, fromDecimal) =>
            oneOrFail(microarcseconds, microseconds, milliarcseconds, milliseconds, arcseconds, seconds, arcminutes, minutes, degrees, hours, dms, hms, fromLong, fromDecimal)
        }
    }
}

