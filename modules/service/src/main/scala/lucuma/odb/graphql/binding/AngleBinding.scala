// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package binding

import lucuma.core.math.Angle
import lucuma.odb.graphql.util.Bindings._

object AngleBinding {

  val Microarcseconds: Matcher[Angle] =
    LongBinding.map(Angle.fromMicroarcseconds)

  val Degrees: Matcher[Angle] =
    BigDecimalBinding.map(bd => Angle.fromDoubleDegrees(bd.toDouble))

  val Dms: Matcher[Angle] =
    StringBinding.emap { s =>
      Angle.fromStringDMS.getOption(s).toRight(s"Invalid angle: $s")
    }

}
