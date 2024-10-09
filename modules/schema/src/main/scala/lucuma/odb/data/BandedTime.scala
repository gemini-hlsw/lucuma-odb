// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.Order
import lucuma.core.enums.ScienceBand
import lucuma.core.model.sequence.CategorizedTime

case class BandedTime(
  band: Option[ScienceBand],
  time: CategorizedTime
)

object BandedTime:
  val Empty: BandedTime = BandedTime(None, CategorizedTime.Zero)

  given Order[BandedTime] =
    Order.by(a => (a.band, a.time))