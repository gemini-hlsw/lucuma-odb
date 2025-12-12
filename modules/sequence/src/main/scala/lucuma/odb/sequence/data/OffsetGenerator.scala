// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.data

import cats.Eq
import cats.Monad
import cats.data.NonEmptyList
import cats.derived.*
import cats.effect.Sync
import cats.effect.std.Random as CatsRandom
import cats.syntax.applicative.*
import cats.syntax.eq.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.StepGuideState
import lucuma.core.geom.OffsetGenerator as OffsetGeneratorImpl
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

  def generate[F[_]: Sync](
    count: NonNegInt,
    seed:  Long = 0L,
    defaultGuideState: StepGuideState = StepGuideState.Enabled
  ): F[List[TelescopeConfig]] =
    def withSeededRandom(
      fa: (Monad[F], CatsRandom[F]) ?=> F[NonEmptyList[Offset]]
    ): F[List[TelescopeConfig]] =
      CatsRandom.scalaUtilRandomSeedLong(seed).flatMap: r =>
        given CatsRandom[F] = r
        fa.map(_.toList.map(o => TelescopeConfig(o, defaultGuideState)))

    PosInt.unapply(count.value).fold(List.empty[TelescopeConfig].pure[F]): posN =>
      this match
        case OffsetGenerator.NoGenerator          =>
          List.empty[TelescopeConfig].pure[F]

        case OffsetGenerator.Enumerated(lst)      =>
          // Enumerated positions come with an explicit guide state so the
          // default is ignored.
          LazyList.continually(lst.toList).flatten.take(count.value).toList.pure[F]

        case OffsetGenerator.Uniform(a, b)        =>
          val w = (a.p.toSignedDecimalArcseconds - b.p.toSignedDecimalArcseconds).abs
          val h = (a.q.toSignedDecimalArcseconds - b.q.toSignedDecimalArcseconds).abs

          val (rows, cols) =
            if h <= 0.000001 then
              (1, posN.value)
            else
              val aspectRatio = w / h

              val cols0 = 1 max Math.sqrt(posN.value * aspectRatio.doubleValue).round.toInt
              val rows  = 1 max (posN.value.toDouble / cols0).ceil.toInt
              val cols  = (posN.value.toDouble / rows).ceil.toInt
              (rows, cols)

          val stepP = if cols <= 2 then w else w / (cols - 1) // arcseconds
          val stepQ = if rows <= 2 then h else h / (rows - 1) // arcseconds

          val p0 = a.p.toSignedDecimalArcseconds max b.p.toSignedDecimalArcseconds
          val q0 = a.q.toSignedDecimalArcseconds max b.q.toSignedDecimalArcseconds
          val o  = Offset.signedDecimalArcseconds.reverseGet((p0, q0))

          val offsets =
            (0 until rows).toList.flatMap: r =>
              (0 until cols).toList.map: c =>
                o + Offset.signedDecimalArcseconds.reverseGet(-stepP * c, -stepQ * r)

          offsets.take(count.value).map(o => TelescopeConfig(o, defaultGuideState)).pure[F]

        case OffsetGenerator.Random(size, center) =>
          withSeededRandom:
            OffsetGeneratorImpl.random(posN, size, center)

        case OffsetGenerator.Spiral(size, center) =>
          withSeededRandom:
            OffsetGeneratorImpl.spiral(posN, size, center)

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