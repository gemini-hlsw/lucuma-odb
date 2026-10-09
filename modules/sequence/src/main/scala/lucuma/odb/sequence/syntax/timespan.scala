// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.syntax

import lucuma.core.util.TimeSpan

trait ToTimeSpanOps:

  extension (self: TimeSpan)
    /** Minutes to two decimal places, for error messages. */
    def toRoundedMinutes: BigDecimal =
      self.toMinutes.setScale(2, BigDecimal.RoundingMode.HALF_UP)

object timespan extends ToTimeSpanOps
