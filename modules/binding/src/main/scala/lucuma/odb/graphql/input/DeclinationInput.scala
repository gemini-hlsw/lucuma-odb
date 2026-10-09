// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import grackle.syntax.*
import lucuma.core.math.Angle
import lucuma.core.math.Declination
import lucuma.odb.graphql.binding.*

object DeclinationInput {

  val Binding: Matcher[Declination] =
    OneOfBinding(
      "microarcseconds" -> AngleBinding.Microarcseconds,
      "degrees"         -> AngleBinding.Degrees,
      "dms"             -> AngleBinding.Dms
    ).rmap: a =>
      Declination.fromAngle.getOption(a).toResult(s"Invalid declination: ${Angle.fromStringDMS.reverseGet(a)}")

}
