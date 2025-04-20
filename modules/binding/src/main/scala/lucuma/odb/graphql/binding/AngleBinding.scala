// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import lucuma.core.math.Angle

object AngleBinding {

  val Microarcseconds: Matcher[Angle] =
    LongBinding.map(Angle.fromMicroarcseconds)
    
  val Milliarcseconds: Matcher[Angle] =
    BigDecimalBinding.map(bd => Angle.fromBigDecimalArcseconds(bd.bigDecimal.movePointLeft(3)))
    
  val Arcseconds: Matcher[Angle] =
    BigDecimalBinding.map(bd => Angle.fromBigDecimalArcseconds(bd))

  val Degrees: Matcher[Angle] =
    BigDecimalBinding.map(bd => Angle.fromDoubleDegrees(bd.toDouble))

  val Dms: Matcher[Angle] =
    StringBinding.emap { s =>
      Angle.fromStringDMS.getOption(s).toRight(s"Invalid angle: $s")
    }

}
