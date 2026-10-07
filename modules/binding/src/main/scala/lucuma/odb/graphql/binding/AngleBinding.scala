// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import lucuma.core.math.Angle

import scala.math.BigDecimal.RoundingMode

object AngleBinding {

  /**
   * `bd` units scaled to the nearest whole sub-unit, modulo `perCircle`. The
   * reduction happens before the conversion to Long, so large inputs wrap
   * around the circle instead of overflowing.
   */
  private[binding] def rounded(bd: BigDecimal, perUnit: Long, perCircle: Long): Long =
    ((bd * perUnit).setScale(0, RoundingMode.HALF_UP) % perCircle).toLong

  private def decimal(µasPerUnit: Long): Matcher[Angle] =
    BigDecimalBinding.map(bd => Angle.fromMicroarcseconds(rounded(bd, µasPerUnit, Angle.µasPer360)))

  val Microarcseconds: Matcher[Angle] =
    LongBinding.map(Angle.fromMicroarcseconds)

  val Milliarcseconds: Matcher[Angle] =
    decimal(1_000L)

  val Arcseconds: Matcher[Angle] =
    decimal(1_000_000L)

  val Arcminutes: Matcher[Angle] =
    decimal(60_000_000L)

  val Degrees: Matcher[Angle] =
    decimal(Angle.µasPerDegree)

  val Dms: Matcher[Angle] =
    StringBinding.emap { s =>
      Angle.fromStringDMS.getOption(s).toRight(s"Invalid angle: $s")
    }

}
