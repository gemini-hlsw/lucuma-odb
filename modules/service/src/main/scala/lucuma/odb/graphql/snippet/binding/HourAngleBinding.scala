// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.snippet
package binding

import lucuma.core.math.HourAngle
import lucuma.odb.graphql.util.Bindings._

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
