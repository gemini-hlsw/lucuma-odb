// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package binding

import lucuma.core.math.Angle
import lucuma.core.math.HourAngle

object HourAngleBinding {

  private val µsPerHour: Long = 3_600_000_000L
  private val µsPer24h: Long  = 24L * µsPerHour

  private def decimal(µsPerUnit: Long): Matcher[HourAngle] =
    BigDecimalBinding.map(bd => HourAngle.fromMicroseconds(AngleBinding.rounded(bd, µsPerUnit, µsPer24h)))

  val Microarcseconds: Matcher[HourAngle] =
    LongBinding.map(Angle.fromMicroarcseconds).map(Angle.hourAngle.get)

  val Microseconds: Matcher[HourAngle] =
    LongBinding.map(HourAngle.fromMicroseconds)

  val DecimalMicroseconds: Matcher[HourAngle] =
    decimal(1L)

  val Milliseconds: Matcher[HourAngle] =
    decimal(1_000L)

  val Seconds: Matcher[HourAngle] =
    decimal(1_000_000L)

  val Minutes: Matcher[HourAngle] =
    decimal(60_000_000L)

  // 1° is 1/15 h, so it is rounded straight to the nearest µs.
  val Degrees: Matcher[HourAngle] =
    decimal(µsPerHour / 15L)

  val Hours: Matcher[HourAngle] =
    decimal(µsPerHour)

  val Hms: Matcher[HourAngle] =
    StringBinding.emap { s =>
      HourAngle.fromStringHMS.getOption(s).toRight(s"Invalid hour angle: $s")
    }

}
