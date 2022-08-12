// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.all._
import edu.gemini.grackle.Result
import lucuma.core.math.Angle
import lucuma.core.math.Declination
import lucuma.odb.graphql.binding.AngleBinding
import lucuma.odb.graphql.util.Bindings._

object DeclinationInput {

  val Binding: Matcher[Declination] =
    ObjectFieldsBinding.rmap {
      case List(
        AngleBinding.Microarcseconds.Option("microarcseconds", rMicroarcseconds),
        AngleBinding.Degrees.Option("degrees", rDegrees),
        AngleBinding.Dms.Option("dms", rDms),
      ) => (rMicroarcseconds, rDegrees, rDms).parTupled.flatMap {
        case (microarcseconds, degrees, hms) =>
          List(microarcseconds, degrees, hms).flatten match {
            case List(a) =>
              Result.fromOption(Declination.fromAngle.getOption(a), s"Invalid declination: ${Angle.fromStringDMS.reverseGet(a)}")
            case as => Result.failure(s"Expected exactly one declination format; found ${as.length}.")
          }
      }
    }

}
