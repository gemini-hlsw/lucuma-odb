// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

type ObservingMode =
  gmos.longslit.Config.GmosNorth |
  gmos.longslit.Config.GmosSouth

extension (m: ObservingMode) {
  def hashBytes: Array[Byte] =
    m match {
      case gn: gmos.longslit.Config.GmosNorth => gn.hashBytes
      case gs: gmos.longslit.Config.GmosSouth => gs.hashBytes
    }
}


