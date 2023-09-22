// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import cats.data.NonEmptyList
import lucuma.odb.sequence.gmos.longslit.Config.GmosNorth
import lucuma.odb.sequence.gmos.longslit.Config.GmosSouth
import lucuma.odb.sequence.util.HashBytes

/**
 * All observing mode options.
 */
type ObservingMode = GmosNorth | GmosSouth

object ObservingMode {

  def reconcile(modes: NonEmptyList[ObservingMode]): Option[ObservingMode] =
    modes.head match {
        case gn: GmosNorth => GmosNorth.reconcile(gn, modes.tail)
        case gs: GmosSouth => GmosSouth.reconcile(gs, modes.tail)
    }

  given HashBytes[ObservingMode] with {
    def hashBytes(a: ObservingMode): Array[Byte] =
      a match {
        case gn: GmosNorth => gn.hashBytes
        case gs: GmosSouth => gs.hashBytes
      }
  }

}
