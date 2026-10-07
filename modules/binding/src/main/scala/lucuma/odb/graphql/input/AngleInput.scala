// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.HourAngle
import lucuma.odb.graphql.binding.*

object AngleInput {

  def getMicroarcseconds(d: Long)  = Angle.microarcseconds.reverseGet(d)
  def getMicroseconds(d: Long)     = HourAngle.fromMicroseconds(d)

  def getDMS(s: String)            = Angle.fromStringDMS.getOption(s).toRight(s"Invalid DMS angle: $s")
  def getHMS(s: String)            = HourAngle.fromStringHMS.getOption(s).toRight(s"Invalid HMS angle: $s")

  // Decimal values are rounded to the nearest whole µas (arc units) or µs (time units).
  val Microarcseconds = AngleBinding.Microarcseconds
  val Microseconds    = HourAngleBinding.DecimalMicroseconds
  val Milliarcseconds = AngleBinding.Milliarcseconds
  val Milliseconds    = HourAngleBinding.Milliseconds
  val ArcSeconds      = AngleBinding.Arcseconds
  val Seconds         = HourAngleBinding.Seconds
  val ArcMinutes      = AngleBinding.Arcminutes
  val Minutes         = HourAngleBinding.Minutes
  val Degrees         = AngleBinding.Degrees
  val Hours           = HourAngleBinding.Hours
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
        HMS.Option("hms", rHMS),
      ) =>
        (rMicroarcseconds, rMicroseconds, rMilliarcseconds, rMilliseconds, rArcSeconds, rSeconds, rArcMinutes, rMinutes, rDegrees, rHours, rDMS, rHMS).parTupled.flatMap {
          case (microarcseconds, microseconds, milliarcseconds, milliseconds, arcseconds, seconds, arcminutes, minutes, degrees, hours, dms, hms) =>
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
