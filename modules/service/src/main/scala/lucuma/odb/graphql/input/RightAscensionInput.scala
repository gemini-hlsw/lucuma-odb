// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.all.*
import grackle.Result
import lucuma.core.math.RightAscension
import lucuma.odb.graphql.binding.*

object RightAscensionInput {

  val Binding: Matcher[RightAscension] =
    ObjectFieldsBinding.rmap {
      case List(
        HourAngleBinding.Microseconds.Option("microseconds", rMicroseconds),
        HourAngleBinding.Degrees.Option("degrees", rDegrees),
        HourAngleBinding.Hours.Option("hours", rHours),
        HourAngleBinding.Hms.Option("hms", rHms),
      ) => (rMicroseconds, rDegrees, rHours, rHms).parTupled.flatMap {
        case (microseconds, degrees, hours, hms) =>
          List(microseconds, degrees, hours, hms).flatten match {
            case List(ha) => Result(RightAscension(ha))
            case has => Matcher.validationFailure(s"Expected exactly one right ascension format; found ${has.length}.")
          }
      }
    }
}
