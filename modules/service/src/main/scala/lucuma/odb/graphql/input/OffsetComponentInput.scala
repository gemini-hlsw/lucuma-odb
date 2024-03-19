// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.all.*
import grackle.Result
import lucuma.core.math.Angle
import lucuma.core.math.Offset.Component
import lucuma.core.math.Offset.P
import lucuma.core.math.Offset.Q
import lucuma.odb.graphql.binding.*
import monocle.Iso

object OffsetComponentInput {

  private def componentBinding[A](
    name: String,
    componentIso: Iso[Component[A], Angle]
  ): Matcher[Component[A]] =
    ObjectFieldsBinding.rmap {
      case List(
        AngleBinding.Microarcseconds.Option("microarcseconds", rMicroarcseconds),
        AngleBinding.Milliarcseconds.Option("milliarcseconds", rMilliarcseconds),
        AngleBinding.Arcseconds.Option("arcseconds", rArcseconds)
      ) => (rMicroarcseconds, rMilliarcseconds, rArcseconds).parTupled.flatMap {
        case (microarcseconds, milliarcseconds, arcseconds) =>
          List(microarcseconds, milliarcseconds, arcseconds).flatten match {
            case List(a) => Result(componentIso.reverseGet(a))
            case as      => Matcher.validationFailure(s"Expected exactly one offset in $name; found ${as.length}.")
          }
      }
    }

  val BindingP: Matcher[P] =
    componentBinding("p", P.angle)

  val BindingQ: Matcher[Q] =
    componentBinding("q", Q.angle)

}