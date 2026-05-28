// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.ghost.ifu

import cats.data.NonEmptyList
import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.option.*
import lucuma.core.enums.GhostResolutionMode
import lucuma.core.geom.ghost.*
import lucuma.core.geom.jts.interpreter.given
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.core.model.Target
import lucuma.core.model.sequence.ghost.GhostIfuMapping
import lucuma.core.util.Timestamp

object GhostIfuMappingSyntax:

  private def ifuAssignment(
    base:          Coordinates,
    c1:            Coordinates,
    c2:            Option[Coordinates],
    positionAngle: Option[Angle]
  ): Int =
    val angle  = positionAngle.getOrElse(Angle.Angle0)

    val field1 = GhostIfuPatrolField.ifu1PatrolFieldAt(angle, Offset.Zero)
    val field2 = GhostIfuPatrolField.ifu2PatrolFieldAt(angle, Offset.Zero)

    val shape1 = field1.eval
    val shape2 = field2.eval

    val pos1 = base.diff(c1).offset
    val pos2 = c2.map(base.diff(_).offset)

    if shape1.contains(pos1) && pos2.forall(shape2.contains)      then -1
    else if pos2.forall(shape1.contains) && shape2.contains(pos1) then  1
    else 0

  private def deriveOneTarget(
    ctx:    IfuMappingContext,
    target: Target
  ): Either[String, GhostIfuMapping] =
    ctx.resolutionMode match
      case GhostResolutionMode.Standard =>
        target match
          case Target.Sidereal(_, track, _, _) =>
            track.at(ctx.when.toInstant).fold("Cannot determine the target coordinates.".asLeft): c =>
              ifuAssignment(ctx.explicitBase.getOrElse(c), c, ctx.sky, ctx.angle) match
                case -1 => ctx.sky.fold(GhostIfuMapping.SingleTarget(track).asRight)(s => GhostIfuMapping.TargetPlusSky(track, s).asRight)
                case  1 => ctx.sky.fold("The target does not fall in range of GHOST IFU1 probe.".asLeft)(s => GhostIfuMapping.SkyPlusTarget(s, track).asRight)
                case  _ => "The target and sky positions are too far apart.".asLeft

          case Target.Nonsidereal(_, _, _)     =>
            ctx.sky.fold(GhostIfuMapping.Nonsidereal.asRight): _ =>
              "GHOST does not support sky positions for nonsidereal targets.".asLeft

          case Target.Opportunity(_, _, _)     =>
            "A GHOST IFU mapping can only be determined after the science target is identified.".asLeft

      case GhostResolutionMode.High =>
        target match
          case Target.Sidereal(_, track, _, _) =>
            track.at(ctx.when.toInstant).fold("Cannot determine the target coordinates.".asLeft): c =>
              ctx.sky.fold("GHOST High Resolution mode requires a sky position.".asLeft): s =>
                ifuAssignment(ctx.explicitBase.getOrElse(c), c, s.some, ctx.angle) match
                  case -1 => GhostIfuMapping.TargetPlusSky(track, s).asRight
                  case  _ => "The target and/or sky position is not reachable by the GHOST IFU probes.".asLeft

          case Target.Nonsidereal(_, _, _)     =>
            ctx.sky.fold(GhostIfuMapping.Nonsidereal.asRight): _ =>
              "GHOST does not support sky positions for nonsidereal targets.".asLeft

          case Target.Opportunity(_, _, _)     =>
            "A GHOST IFU mapping can only be determined after the science target is identified.".asLeft

  private def deriveDualTarget(
    ctx:     IfuMappingContext,
    targetA: Target,
    targetB: Target
  ): Either[String, GhostIfuMapping] =
    (ctx.resolutionMode, ctx.sky) match
      case (GhostResolutionMode.Standard, None)    =>
        (targetA, targetB) match
          case (Target.Sidereal(_, track1, _, _), Target.Sidereal(_, track2, _, _)) =>
            (track1.at(ctx.when.toInstant), track2.at(ctx.when.toInstant))
              .tupled
              .fold("Cannot determine the target coordinates.".asLeft): (c1, c2) =>
                val base = ctx.explicitBase.getOrElse(Coordinates.centerOf(NonEmptyList.of(c1, c2)))
                ifuAssignment(base, c1, c2.some, ctx.angle) match
                  case -1 => GhostIfuMapping.DualTarget(track1, track2).asRight
                  case  1 => GhostIfuMapping.DualTarget(track2, track1).asRight
                  case  _ => "The targets do not fall in range of the GHOST IFU probes.".asLeft
          case _                                                                    =>
            "GHOST Dual Target mode is available for sidereal targets only.".asLeft

      case (GhostResolutionMode.Standard, Some(_)) =>
        "A sky position should not be defined for Dual Target mode.".asLeft

      case (GhostResolutionMode.High, _)           =>
        "Dual Target mode is only available in Standard Resolution.".asLeft


  extension (g: GhostIfuMapping.type)
    def derive(
      ctx:     IfuMappingContext,
      targets: List[Target]
    ): Either[String, GhostIfuMapping] =
      targets match
        case Nil          => "Cannot derive a GHOST IFU mapping until targets are defined.".asLeft
        case List(t)      => deriveOneTarget(ctx, t)
        case List(t1, t2) => deriveDualTarget(ctx, t1, t2)
        case _            => "Cannot derive a GHOST IFU mapping with more than two targets.".asLeft

    def validate(
      ctx:     IfuMappingContext,
      targets: List[Target]
    ): Option[String] =
      derive(ctx, targets).swap.toOption