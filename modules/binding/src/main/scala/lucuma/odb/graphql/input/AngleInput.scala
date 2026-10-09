// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

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
    OneOfBinding(
      "microarcseconds" -> Microarcseconds,
      "microseconds"    -> Microseconds,
      "milliarcseconds" -> Milliarcseconds,
      "milliseconds"    -> Milliseconds,
      "arcseconds"      -> ArcSeconds,
      "seconds"         -> Seconds,
      "arcminutes"      -> ArcMinutes,
      "minutes"         -> Minutes,
      "degrees"         -> Degrees,
      "hours"           -> Hours,
      "dms"             -> DMS,
      "hms"             -> HMS
    )
}
