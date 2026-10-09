// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import lucuma.core.math.RightAscension
import lucuma.odb.graphql.binding.*

object RightAscensionInput {

  val Binding: Matcher[RightAscension] =
    OneOfBinding(
      "microseconds" -> HourAngleBinding.Microseconds,
      "degrees"      -> HourAngleBinding.Degrees,
      "hours"        -> HourAngleBinding.Hours,
      "hms"          -> HourAngleBinding.Hms
    ).map(RightAscension(_))
}
