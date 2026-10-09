// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import coulomb.*
import coulomb.syntax.withUnit
import lucuma.core.math.ProperMotion
import lucuma.core.math.VelocityAxis
import lucuma.core.math.units.*
import lucuma.core.util.*
import lucuma.odb.graphql.binding.*

object ProperMotionComponentInput {

  object RA {
    val Binding: Matcher[ProperMotion.RA] =
      binding[VelocityAxis.RA]
  }

  object Dec {
    val Binding: Matcher[ProperMotion.Dec] =
      binding[VelocityAxis.Dec]
  }

  private def binding[A]: Matcher[ProperMotion.AngularVelocity Of A] =
    OneOfBinding(
      "microarcsecondsPerYear" -> LongBinding.map(_.withUnit[MicroArcSecondPerYear]),
      "milliarcsecondsPerYear" -> BigDecimalBinding.map(n => (n * 1000).toLong.withUnit[MicroArcSecondPerYear])
    ).map(a => ProperMotion.AngularVelocity(a).tag[A])

}
