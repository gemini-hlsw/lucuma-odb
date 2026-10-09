// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import lucuma.core.math.Angle
import lucuma.core.math.Offset.Component
import lucuma.core.math.Offset.P
import lucuma.core.math.Offset.Q
import lucuma.odb.graphql.binding.*
import monocle.Iso

object OffsetComponentInput {

  private def componentBinding[A](
    componentIso: Iso[Component[A], Angle]
  ): Matcher[Component[A]] =
    OneOfBinding(
      "microarcseconds" -> AngleBinding.Microarcseconds,
      "milliarcseconds" -> AngleBinding.Milliarcseconds,
      "arcseconds"      -> AngleBinding.Arcseconds
    ).map(componentIso.reverseGet)

  val BindingP: Matcher[P] =
    componentBinding(P.angle)

  val BindingQ: Matcher[Q] =
    componentBinding(Q.angle)

}