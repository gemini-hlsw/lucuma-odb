// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.Order
import cats.syntax.eq.*

enum ProgramClass(val abbreviation: String):
  case Calibration extends ProgramClass("CAL")
  case Engineering extends ProgramClass("ENG")
  case Example     extends ProgramClass("XPL")
  case Library     extends ProgramClass("LIB")
  case Science     extends ProgramClass("SCI")

object ProgramClass {

  def fromAbbreviation(a: String): Option[ProgramClass] =
    values.find(_.abbreviation === a)

  given Order[ProgramClass] =
    Order.by(_.abbreviation)

  given Ordering[ProgramClass] =
    Order.catsKernelOrderingForOrder

}

