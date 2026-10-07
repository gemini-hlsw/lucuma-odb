// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.data.OptionT
import cats.syntax.all.*
import grackle.Result
import lucuma.core.math.Parallax
import lucuma.odb.graphql.binding.*

object ParallaxModelInput {

  val Binding: Matcher[Parallax] =
    ObjectFieldsBinding.rmap {
      case List(
        LongBinding.Option("microarcseconds", rMicroarcseconds),
        BigDecimalBinding.Option("milliarcseconds", rMilliarcseconds),
      ) =>
        val rMicroarcsecondsʹ = OptionT(rMicroarcseconds).map(Parallax.microarcseconds.reverseGet).value
        val rMilliarcsecondsʹ = OptionT(rMilliarcseconds).map(Parallax.milliarcseconds.reverseGet).value
        (rMicroarcsecondsʹ, rMilliarcsecondsʹ).parTupled.flatMap {
          case (microarcseconds, milliarcseconds) =>
            List(microarcseconds, milliarcseconds).flatten match {
              case List(p) => Result(p)
              case other   => Matcher.validationFailure(s"Expected exactly one parallax representation; found ${other.length}.")
            }
        }
    }

}