// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.snippet
package input

import lucuma.odb.graphql.util.Bindings._
import lucuma.core.math.Parallax
import cats.data.OptionT
import edu.gemini.grackle.Result
import cats.syntax.all._

object ParallaxModelInput {

  val Binding: Matcher[Parallax] =
    ObjectFieldsBinding.rmap {
      case List(
        LongBinding.Option("microarcseconds", rMicroarcseconds),
        BigDecimalBinding.Option("milliarcseconds", rMilliarcseconds),
        ParallaxLongInput.Binding.Option("fromLong", rFromLong),
        ParallaxLongInput.Binding.Option("fromDecimal", rFromDecimal),
      ) =>
        val rMicroarcsecondsʹ = OptionT(rMicroarcseconds).map(Parallax.microarcseconds.reverseGet).value
        val rMilliarcsecondsʹ = OptionT(rMilliarcseconds).map(Parallax.milliarcseconds.reverseGet).value
        (rMicroarcsecondsʹ, rMilliarcsecondsʹ, rFromLong, rFromDecimal).parTupled.flatMap {
          case (microarcseconds, milliarcseconds, fromLong, fromDecimal) =>
            List(microarcseconds, milliarcseconds, fromLong, fromDecimal).flatten match {
              case List(p) => Result(p)
              case other   => Result.failure(s"Expected exactly one parallax representation; found ${other.length}.")
            }
        }
    }

  object ParallaxLongInput {
    val Binding = LongInput("ParallaxUnits") {
      case (value, "MICROARCSECONDS") => Result(Parallax.microarcseconds.reverseGet(value))
      case (value, "MILLIARCSECONDS") => Result(Parallax.microarcseconds.reverseGet(value * 1000L))
    }
  }

  object ParallaxDecimalInput {
    val Binding = DecimalInput("ParallaxUnits") {
      case (value, "MICROARCSECONDS") => Result(Parallax.microarcseconds.reverseGet(value.toLong))
      case (value, "MILLIARCSECONDS") => Result(Result(Parallax.milliarcseconds.reverseGet(value)))
    }
  }

}