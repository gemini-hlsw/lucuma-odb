// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import lucuma.odb.sequence.util.HashBytes

/**
 * All observing mode options.
 */
type ObservingMode =
  gmos.longslit.Config.GmosNorth |
  gmos.longslit.Config.GmosSouth

given HashBytes[ObservingMode] with {
  def hashBytes(a: ObservingMode): Array[Byte] =
    a match {
      case gn: gmos.longslit.Config.GmosNorth => gn.hashBytes
      case gs: gmos.longslit.Config.GmosSouth => gs.hashBytes
    }
}

