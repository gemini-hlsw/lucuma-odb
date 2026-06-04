// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.util

import cats.syntax.either.*
import lucuma.core.enums.GhostBinning
import lucuma.core.enums.GhostIfu1FiberAgitator
import lucuma.core.enums.GhostIfu2FiberAgitator
import lucuma.core.enums.GhostIfuMappingType
import lucuma.core.enums.GhostReadMode
import lucuma.core.enums.GhostResolutionMode
import lucuma.core.math.Coordinates
import lucuma.core.model.sequence.ghost.GhostDetector
import lucuma.core.model.sequence.ghost.GhostDynamicConfig
import lucuma.core.model.sequence.ghost.GhostIfuMapping
import lucuma.core.model.sequence.ghost.GhostStaticConfig
import skunk.*
import skunk.data.Type

trait GhostCodecs:

  import Codecs.coordinates
  import Codecs.enumerated
  import Codecs.int4_pos
  import Codecs.target_id
  import Codecs.time_span

  val ghost_binning: Codec[GhostBinning] =
    enumerated[GhostBinning](Type.varchar)

  val ghost_ifu_mapping_type: Codec[GhostIfuMappingType] =
    enumerated[GhostIfuMappingType](Type("e_ghost_ifu_mapping"))

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

  private enum IfuSlot:
    case Empty
    case Sky(coordinates: Coordinates)
    case Target(id: lucuma.core.model.Target.Id)

  private val ifu_slot: Codec[IfuSlot] =
    (
      coordinates.opt     *:
      target_id.opt
    ).eimap {
      case (None, None)         => IfuSlot.Empty.asRight
      case (Some(coords), None) => IfuSlot.Sky(coords).asRight
      case (None, Some(id))     => IfuSlot.Target(id).asRight
      case other                => s"Inconsistent GHOST IFU mapping: $other".asLeft
    }{
      case IfuSlot.Empty     => (None, None)
      case IfuSlot.Sky(c)    => (Some(c), None)
      case IfuSlot.Target(t) => (None, Some(t))
    }

  val ghost_ifu_mapping: Codec[GhostIfuMapping] =
    (
      ghost_ifu_mapping_type *:
      ifu_slot               *:
      ifu_slot
    ).eimap {
      case (GhostIfuMappingType.SingleTarget, IfuSlot.Target(ifu1), IfuSlot.Empty)      =>
        GhostIfuMapping.SingleTarget(ifu1).asRight
      case (GhostIfuMappingType.TargetPlusSky, IfuSlot.Target(ifu1), IfuSlot.Sky(ifu2)) =>
        GhostIfuMapping.TargetPlusSky(ifu1, ifu2).asRight
      case (GhostIfuMappingType.SkyPlusTarget, IfuSlot.Sky(ifu1), IfuSlot.Target(ifu2)) =>
        GhostIfuMapping.SkyPlusTarget(ifu1, ifu2).asRight
      case (GhostIfuMappingType.DualTarget, IfuSlot.Target(ifu1), IfuSlot.Target(ifu2)) =>
        GhostIfuMapping.DualTarget(ifu1, ifu2).asRight
      case (mappingType, slot1, slot2)                                                  =>
        s"Inconsistent GHOST IFU mapping: $mappingType, $slot1, $slot2".asLeft
    } {
      case GhostIfuMapping.SingleTarget(ifu1)        =>
        (GhostIfuMappingType.SingleTarget, IfuSlot.Target(ifu1), IfuSlot.Empty)
      case GhostIfuMapping.TargetPlusSky(ifu1, ifu2) =>
        (GhostIfuMappingType.TargetPlusSky, IfuSlot.Target(ifu1), IfuSlot.Sky(ifu2))
      case GhostIfuMapping.SkyPlusTarget(ifu1, ifu2) =>
        (GhostIfuMappingType.SkyPlusTarget, IfuSlot.Sky(ifu1), IfuSlot.Target(ifu2))
      case GhostIfuMapping.DualTarget(ifu1, ifu2)    =>
        (GhostIfuMappingType.DualTarget, IfuSlot.Target(ifu1), IfuSlot.Target(ifu2))
    }


  val ghost_static: Codec[GhostStaticConfig] =
    (
      ghost_resolution_mode  *:
      ghost_ifu_mapping      *:
      time_span.opt
    ).to[GhostStaticConfig]

object GhostCodecs extends GhostCodecs