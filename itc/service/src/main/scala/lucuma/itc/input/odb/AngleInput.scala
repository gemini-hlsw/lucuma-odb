// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all.*
import grackle.Result
import lucuma.core.math.Angle
import lucuma.core.math.HourAngle
import lucuma.odb.graphql.binding.*

object AngleInput {

  def getMicroarcseconds(d: Long)   = Angle.microarcseconds.reverseGet(d)
  def getMicroseconds(d:    Long)   = HourAngle.fromMicroseconds(d)
  def getMilliarcseconds(d: Int)    = Angle.milliarcseconds.reverseGet(d)
  def getMilliseconds(d:    Int)    = HourAngle.milliseconds.reverseGet(d)
  def getArcSeconds(d:      Double) = Angle.fromDoubleArcseconds(d)
  def getSeconds(d:         Int)    = HourAngle.seconds.reverseGet(d)
  def getArcMinutes(d:      Int)    = Angle.arcminutes.reverseGet(d)
  def getMinutes(d:         Int)    = HourAngle.minutes.reverseGet(d)
  def getDegrees(d:         Double) = Angle.fromDoubleDegrees(d)
  def getHours(d:           Double) = HourAngle.fromDoubleHours(d)
  def getDMS(s:             String) = Angle.fromStringDMS.getOption(s).toRight(s"Invalid DMS angle: $s")
  def getHMS(s:             String) = HourAngle.fromStringHMS.getOption(s).toRight(s"Invalid HMS angle: $s")

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
            HMS.Option("hms", rHMS)
          ) =>
        (rMicroarcseconds,
         rMicroseconds,
         rMilliarcseconds,
         rMilliseconds,
         rArcSeconds,
         rSeconds,
         rArcMinutes,
         rMinutes,
         rDegrees,
         rHours,
         rDMS,
         rHMS
        ).parTupled.flatMap {
          case (microarcseconds,
                microseconds,
                milliarcseconds,
                milliseconds,
                arcseconds,
                seconds,
                arcminutes,
                minutes,
                degrees,
                hours,
                dms,
                hms
              ) =>
            oneOrFail(
              microarcseconds -> "microarcseconds",
              microseconds    -> "microseconds",
              milliarcseconds -> "milliarcseconds",
              milliseconds    -> "milliseconds",
              arcseconds      -> "arcseconds",
              seconds         -> "seconds",
              arcminutes      -> "arcminutes",
              minutes         -> "minutes",
              degrees         -> "degrees",
              hours           -> "hours",
              dms             -> "dms",
              hms             -> "hms"
            )
        }
    }
}
