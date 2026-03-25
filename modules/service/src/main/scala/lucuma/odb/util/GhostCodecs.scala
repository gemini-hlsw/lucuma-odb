// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.util

import lucuma.core.enums.GhostBinning
import lucuma.core.enums.GhostReadMode
import lucuma.core.enums.GhostResolutionMode
import skunk.*
import skunk.data.Type

trait GhostCodecs:

  import Codecs.enumerated

  val ghost_binning: Codec[GhostBinning] =
    enumerated[GhostBinning](Type.varchar)

  val ghost_read_mode: Codec[GhostReadMode] =
    enumerated[GhostReadMode](Type.varchar)

  val ghost_resolution_mode: Codec[GhostResolutionMode] =
    enumerated[GhostResolutionMode](Type.varchar)

object GhostCodecs extends GhostCodecs