// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.Eq
import cats.derived.*
import lucuma.core.enums.Flamingos2ReadMode
import lucuma.core.enums.GnirsReadMode

/**
 * The detector read mode an ITC result was computed with, for the instruments whose recipe
 * selects one: chosen by the ITC in signal-to-noise mode, the requested one in time-and-count
 * mode.
 */
enum ItcReadMode derives Eq:
  case Gnirs(readMode: GnirsReadMode)
  case Flamingos2(readMode: Flamingos2ReadMode)

object ItcReadMode:
  // From the per-instrument fields of the JSON encoding, at most one of which is set.
  def fromFields(gnirs: Option[GnirsReadMode], flamingos2: Option[Flamingos2ReadMode]): Option[ItcReadMode] =
    gnirs.map(Gnirs.apply).orElse(flamingos2.map(Flamingos2.apply))
