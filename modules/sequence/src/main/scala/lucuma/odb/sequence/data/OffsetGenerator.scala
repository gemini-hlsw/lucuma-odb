// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.data

import cats.Eq
import cats.derived.*
import cats.syntax.eq.*
import cats.syntax.option.*
import cats.data.NonEmptyList
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.odb.sequence.syntax.hash.*
import lucuma.odb.sequence.util.HashBytes
import monocle.Lens
import monocle.Optional
import monocle.Prism
import monocle.macros.GenLens
import monocle.macros.GenPrism

sealed trait OffsetGenerator:

  import OffsetGenerator.*

  def offsetGeneratorType: OffsetGeneratorType =
    this match
      case NoGenerator   => OffsetGeneratorType.NoGenerator
      case Enumerated(_) => OffsetGeneratorType.Enumerated
      case Random(_, _)  => OffsetGeneratorType.Random
      case Spiral(_, _)  => OffsetGeneratorType.Spiral
      case Uniform(_, _) => OffsetGeneratorType.Uniform

object OffsetGenerator:

  case object NoGenerator extends OffsetGenerator

  case class Enumerated(
    values: NonEmptyList[TelescopeConfig]
  ) extends OffsetGenerator derives Eq

  object Enumerated:

    given HashBytes[Enumerated] with
      def hashBytes(e: Enumerated): Array[Byte] =
        e.values.hashBytes

  case class Random(
    size:   Angle,
    center: Offset
  ) extends OffsetGenerator derives Eq

  object Random:
    given HashBytes[Random] with
      def hashBytes(r: Random): Array[Byte] =
        Array.concat(r.getClass.getName.hashBytes, r.size.hashBytes, r.center.hashBytes)

  case class Spiral(
    size:   Angle,
    center: Offset
  ) extends OffsetGenerator derives Eq

  object Spiral:
    given HashBytes[Spiral] with
      def hashBytes(s: Spiral): Array[Byte] =
        Array.concat(s.getClass.getName.hashBytes, s.size.hashBytes, s.center.hashBytes)

  case class Uniform(
    cornerA: Offset,
    cornerB: Offset
  ) extends OffsetGenerator derives Eq

  object Uniform:
    val cornerA: Lens[Uniform, Offset] = GenLens[Uniform](_.cornerA)
    val cornerB: Lens[Uniform, Offset] = GenLens[Uniform](_.cornerB)

    given HashBytes[Uniform] =
      HashBytes.by2(_.cornerA, _.cornerB)

  val uniform: Prism[OffsetGenerator, Uniform]   = GenPrism[OffsetGenerator, Uniform]
  val cornerA: Optional[OffsetGenerator, Offset] = uniform.andThen(Uniform.cornerA)
  val cornerB: Optional[OffsetGenerator, Offset] = uniform.andThen(Uniform.cornerB)

  val size: Optional[OffsetGenerator, Angle] =
    Optional.apply[OffsetGenerator, Angle] {
      case Random(s, _) => s.some
      case Spiral(s, _) => s.some
      case _            => none
    } { s => o =>
      o match
        case Random(_, c) => Random(s, c)
        case Spiral(_, c) => Spiral(s, c)
        case i            => i
    }

  val center: Optional[OffsetGenerator, Offset] =
    Optional.apply[OffsetGenerator, Offset] {
      case Random(_, c) => c.some
      case Spiral(_, c) => c.some
      case _            => none
    } { c => o =>
      o match
        case Random(s, _) => Random(s, c)
        case Spiral(s, _) => Spiral(s, c)
        case i            => i
    }

  given HashBytes[OffsetGenerator] with
    def hashBytes(g: OffsetGenerator): Array[Byte] =
      g match
        case NoGenerator       => Array.empty
        case e @ Enumerated(_) => e.hashBytes
        case r @ Random(_, _)  => r.hashBytes
        case s @ Spiral(_, _)  => s.hashBytes
        case u @ Uniform(_, _) => u.hashBytes

  given Eq[OffsetGenerator] with
    def eqv(x: OffsetGenerator, y: OffsetGenerator): Boolean =
      (x, y) match
        case (NoGenerator, NoGenerator)             => true
        case (a @ Enumerated(_), b @ Enumerated(_)) => a === b
        case (a @ Random(_, _),  b @ Random(_, _))  => a === b
        case (a @ Spiral(_, _),  b @ Spiral(_, _))  => a === b
        case (a @ Uniform(_, _), b @ Uniform(_, _)) => a === b
        case _                                      => false