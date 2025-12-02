// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.data

import cats.syntax.option.*
import cats.data.NonEmptyList
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.model.sequence.TelescopeConfig
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
  ) extends OffsetGenerator

  case class Random(
    size:   Angle,
    center: Offset
  ) extends OffsetGenerator

  case class Spiral(
    size:   Angle,
    center: Offset
  ) extends OffsetGenerator

  case class Uniform(
    cornerA: Offset,
    cornerB: Offset
  ) extends OffsetGenerator

  object Uniform:
    val cornerA: Lens[Uniform, Offset] = GenLens[Uniform](_.cornerA)
    val cornerB: Lens[Uniform, Offset] = GenLens[Uniform](_.cornerB)

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