// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.phase0

import lucuma.core.util.Enumerated

enum Capability(val tag: String, val label: String) derives Enumerated:
  case NodAndShuffle extends Capability("nodandshuffle", "Nod&Shuffle")
  case Coronagraph   extends Capability("coronagraph",   "coronagraph")
