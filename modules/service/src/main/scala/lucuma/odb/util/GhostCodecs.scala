// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.util

import lucuma.core.enums.GhostBinning
import lucuma.core.enums.GhostIfu1FiberAgitator
import lucuma.core.enums.GhostIfu2FiberAgitator
import lucuma.core.enums.GhostReadMode
import lucuma.core.enums.GhostResolutionMode
import lucuma.core.model.sequence.ghost.GhostDetector
import lucuma.core.model.sequence.ghost.GhostDynamicConfig
import lucuma.core.model.sequence.ghost.GhostStaticConfig
import skunk.*
import skunk.data.Type

trait GhostCodecs:

  import Codecs.enumerated
  import Codecs.int4_pos
  import Codecs.time_span

  val ghost_binning: Codec[GhostBinning] =
    enumerated[GhostBinning](Type.varchar)

  val ghost_ifu1_fiber_agitator: Codec[GhostIfu1FiberAgitator] =
    enumerated[GhostIfu1FiberAgitator](Type("e_ghost_fiber_agitator"))

  val ghost_ifu2_fiber_agitator: Codec[GhostIfu2FiberAgitator] =
    enumerated[GhostIfu2FiberAgitator](Type("e_ghost_fiber_agitator"))

  val ghost_read_mode: Codec[GhostReadMode] =
    enumerated[GhostReadMode](Type.varchar)

  val ghost_resolution_mode: Codec[GhostResolutionMode] =
    enumerated[GhostResolutionMode](Type.varchar)

  val ghost_detector: Codec[GhostDetector] =
    (
      time_span     *:
      int4_pos      *:
      ghost_binning *:
      ghost_read_mode
    ).to[GhostDetector]

  val ghost_dynamic: Codec[GhostDynamicConfig] =
    (
      ghost_detector            *:
      ghost_detector            *:
      ghost_ifu1_fiber_agitator *:
      ghost_ifu2_fiber_agitator
    ).imap(
      (red, blue, ifu1, ifu2) => GhostDynamicConfig(GhostDetector.Red(red), GhostDetector.Blue(blue), ifu1, ifu2)
    )(
      dyn => (dyn.red.value, dyn.blue.value, dyn.ifu1FiberAgitator, dyn.ifu2FiberAgitator)
    )

  val ghost_static: Codec[GhostStaticConfig] =
    ghost_resolution_mode.to[GhostStaticConfig]

object GhostCodecs extends GhostCodecs