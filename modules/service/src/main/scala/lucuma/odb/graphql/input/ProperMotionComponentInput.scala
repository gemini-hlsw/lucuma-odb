// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.data.Nested
import cats.syntax.all._
import coulomb._
import coulomb.syntax.withUnit
import grackle.Result
import lucuma.core.math.ProperMotion
import lucuma.core.math.VelocityAxis
import lucuma.core.math.units._
import lucuma.core.util.*
import lucuma.odb.graphql.binding._

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
    ObjectFieldsBinding.rmap {
      case List(
        LongBinding.Option("microarcsecondsPerYear", rMicroarcsecondsPerYear),
        BigDecimalBinding.Option("milliarcsecondsPerYear", rMilliarcsecondsPerYear),
     ) =>
        val rMicroarcsecondsPerYearʹ = Nested(rMicroarcsecondsPerYear).map(_.withUnit[MicroArcSecondPerYear]).value
        val rMilliarcsecondsPerYearʹ = Nested(rMilliarcsecondsPerYear).map(n => (n * 1000).toLong.withUnit[MicroArcSecondPerYear]).value
        (rMicroarcsecondsPerYearʹ, rMilliarcsecondsPerYearʹ).parTupled.flatMap {
          case (microarcsecondsPerYear, milliarcsecondsPerYear) =>
            List(microarcsecondsPerYear, milliarcsecondsPerYear).flatten match {
              case List(a) => Result(ProperMotion.AngularVelocity(a).tag[A])
              case as => Matcher.validationFailure(s"Expected exactly one proper motion component format; found ${as.length}.")
            }
        }
    }

}
