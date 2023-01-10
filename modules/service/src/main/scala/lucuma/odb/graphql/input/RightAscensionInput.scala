// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.all._
import edu.gemini.grackle.Result
import lucuma.core.math.RightAscension
import lucuma.odb.graphql.binding._

object RightAscensionInput {

  val Binding: Matcher[RightAscension] =
    ObjectFieldsBinding.rmap {
      case List(
        HourAngleBinding.Microarcseconds.Option("microarcseconds", rMicroarcseconds),
        HourAngleBinding.Degrees.Option("degrees", rDegrees),
        HourAngleBinding.Hours.Option("hours", rHours),
        HourAngleBinding.Hms.Option("hms", rHms),
      ) => (rMicroarcseconds, rDegrees, rHours, rHms).parTupled.flatMap {
        case (microarcseconds, degrees, hours, hms) =>
          List(microarcseconds, degrees, hours, hms).flatten match {
            case List(ha) => Result(RightAscension(ha))
            case has => Result.failure(s"Expected exactly one right ascension format; found ${has.length}.")
          }
      }
    }
}
