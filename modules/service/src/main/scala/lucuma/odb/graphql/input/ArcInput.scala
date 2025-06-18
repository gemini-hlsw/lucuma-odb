// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.all.*
import grackle.Result
import lucuma.core.enums.ArcType
import lucuma.core.math.Angular
import lucuma.core.math.Arc
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.odb.graphql.binding.*

object ArcInput:

  def binding[A: Angular](a: Matcher[A]): Matcher[Arc[A]] =
    val ArcTypeBinding = enumeratedBinding[ArcType]
    val ElementBinding = a
    ObjectFieldsBinding.rmap:
      case List(
        ArcTypeBinding("type", rTpe),
        ElementBinding.Option("start", rStart),
        ElementBinding.Option("end", rEnd)
      ) =>
        (rTpe, rStart, rEnd).parTupled.flatMap:
          case (ArcType.Empty, None, None) => Result(Arc.Empty())
          case (ArcType.Full, None, None) => Result(Arc.Full())
          case (ArcType.Partial, Some(s), Some(e)) => Result(Arc.Partial(s, e))
          case (ArcType.Full | ArcType.Empty, _, _) => Matcher.validationFailure(s"Full and empty arcs must not specify start or end.")
          case (ArcType.Partial, _, _) => Matcher.validationFailure(s"Partial arcs must specify both start and end.")

object RightAscensionArcInput:
  val Binding: Matcher[Arc[RightAscension]] =
    ArcInput.binding(RightAscensionInput.Binding)

object DeclinationArcInput:
  val Binding: Matcher[Arc[Declination]] =
    ArcInput.binding(DeclinationInput.Binding)
