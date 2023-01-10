// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.all._
import edu.gemini.grackle.Result
import lucuma.core.math.Angle
import lucuma.core.math.Offset.Q
import lucuma.odb.graphql.binding._

object OffsetComponentInput {

  val Binding: Matcher[Q] =
    ObjectFieldsBinding.rmap {
      case List(
        AngleBinding.Microarcseconds.Option("microarcseconds", rMicroarcseconds),
        AngleBinding.Milliarcseconds.Option("milliarcseconds", rMilliarcseconds),
        AngleBinding.Arcseconds.Option("arcseconds", rArcseconds)
      ) => (rMicroarcseconds, rMilliarcseconds, rArcseconds).parTupled.flatMap {
        case (microarcseconds, milliarcseconds, arcseconds) =>
          List(microarcseconds, milliarcseconds, arcseconds).flatten match {
            case List(a) => Result(Q.angle.reverseGet(a))
            case as      => Result.failure(s"Expected exactly one offset in q; found ${as.length}.")
          }
      }
    }

}
