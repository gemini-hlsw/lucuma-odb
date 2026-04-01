// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.util

import cats.syntax.option.*
import lucuma.core.enums.GhostBinning
import lucuma.core.enums.GhostReadMode
import lucuma.core.enums.GhostResolutionMode
import lucuma.core.model.sequence.ghost.GhostDetector
import lucuma.core.model.sequence.ghost.GhostDynamicConfig
import lucuma.core.model.sequence.ghost.GhostStaticConfig
import lucuma.core.model.sequence.ghost.Ifu1FiberAgitator
import lucuma.core.model.sequence.ghost.Ifu2FiberAgitator
import skunk.*
import skunk.codec.`enum`.`enum`
import skunk.data.Type

trait GhostCodecs:

  import Codecs.enumerated
  import Codecs.int4_pos
  import Codecs.time_span

  val ghost_binning: Codec[GhostBinning] =
    enumerated[GhostBinning](Type.varchar)

  private def ghost_fiber_agitator[A](
    toBoolean: A => Boolean,
    enabled:   A,
    disabled:  A
  ): Codec[A] =

    val toString: A => String =
      a => if toBoolean(a) then "enabled" else "disabled"

    val fromString: String => Option[A] = {
      case "enabled"  => enabled.some
      case "disabled" => disabled.some
      case _          => none
    }

    `enum`(toString, fromString, Type("e_ghost_fiber_agitator"))

  val ghost_ifu1_fiber_agitator: Codec[Ifu1FiberAgitator] =
    ghost_fiber_agitator(_.value, Ifu1FiberAgitator.Enabled, Ifu1FiberAgitator.Disabled)

  val ghost_ifu2_fiber_agitator: Codec[Ifu2FiberAgitator] =
    ghost_fiber_agitator(_.value, Ifu2FiberAgitator.Enabled, Ifu2FiberAgitator.Disabled)

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
      dyn => (dyn.redCamera.value, dyn.blueCamera.value, dyn.ifu1FiberAgitator, dyn.ifu2FiberAgitator)
    )

  val ghost_static: Codec[GhostStaticConfig] =
    ghost_resolution_mode.to[GhostStaticConfig]

object GhostCodecs extends GhostCodecs