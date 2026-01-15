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
import lucuma.core.enums.TelescopeConfigGeneratorType
import lucuma.core.geom.OffsetGenerator
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.odb.sequence.syntax.hash.*
import lucuma.odb.sequence.util.HashBytes
import monocle.Iso
import monocle.Lens
import monocle.Optional
import monocle.Prism
import monocle.macros.GenLens
import monocle.macros.GenPrism

sealed trait TelescopeConfigGenerator:

  import TelescopeConfigGenerator.*

  def offsetGeneratorType: TelescopeConfigGeneratorType =
    this match
      case NoGenerator   => TelescopeConfigGeneratorType.NoGenerator
      case Enumerated(_) => TelescopeConfigGeneratorType.Enumerated
      case Random(_, _)  => TelescopeConfigGeneratorType.Random
      case Spiral(_, _)  => TelescopeConfigGeneratorType.Spiral
      case Uniform(_)    => TelescopeConfigGeneratorType.Uniform

  def generate[F[_]: Sync](
    count: NonNegInt,
    defaultGuideState: StepGuideState = StepGuideState.Enabled
  ): F[List[TelescopeConfig]] =
    def withSeededRandom(seed: Long)(
      fa: (Monad[F], CatsRandom[F]) ?=> F[NonEmptyList[Offset]]
    ): F[List[TelescopeConfig]] =
      CatsRandom.scalaUtilRandomSeedLong(seed).flatMap: r =>
        given CatsRandom[F] = r
        fa.map(_.toList.map(o => TelescopeConfig(o, defaultGuideState)))

    PosInt.unapply(count.value).fold(List.empty[TelescopeConfig].pure[F]): posN =>
      this match
        case NoGenerator     =>
          List.empty[TelescopeConfig].pure[F]

        case Enumerated(lst) =>
          // Enumerated positions come with an explicit guide state so the
          // default is ignored.
          LazyList.continually(lst.toList).flatten.take(count.value).toList.pure[F]

        case Random(og, s)   =>
          withSeededRandom(s):
            og.generate[F](posN)

        case Spiral(og, s)   =>
          withSeededRandom(s):
            og.generate[F](posN)

        case Uniform(og)     =>
          withSeededRandom(0L):
            og.generate[F](posN)

object TelescopeConfigGenerator:

  case object NoGenerator extends TelescopeConfigGenerator

  case class Enumerated(
    values: NonEmptyList[TelescopeConfig]
  ) extends TelescopeConfigGenerator derives Eq

  object Enumerated:

    given HashBytes[Enumerated] with
      def hashBytes(e: Enumerated): Array[Byte] =
        e.values.hashBytes

  val enumerated: Prism[TelescopeConfigGenerator, Enumerated] =
    GenPrism[TelescopeConfigGenerator, Enumerated]

  case class Random(
    offsetGenerator: OffsetGenerator.Random,
    seed:            Long
  ) extends TelescopeConfigGenerator derives Eq

  object Random:

    given HashBytes[Random] with
      def hashBytes(r: Random): Array[Byte] =
        Array.concat(
          r.offsetGenerator.getClass.getName.hashBytes,
          r.offsetGenerator.size.hashBytes,
          r.offsetGenerator.center.hashBytes,
          r.seed.hashBytes
        )

    val offsetGenerator: Lens[Random, OffsetGenerator.Random] =
      GenLens[Random](_.offsetGenerator)

    val seed: Lens[Random, Long] =
      GenLens[Random](_.seed)

  val random: Prism[TelescopeConfigGenerator, Random] =
    GenPrism[TelescopeConfigGenerator, Random]


  case class Spiral(
    offsetGenerator: OffsetGenerator.Spiral,
    seed:            Long
  ) extends TelescopeConfigGenerator derives Eq

  object Spiral:

    given HashBytes[Spiral] with
      def hashBytes(s: Spiral): Array[Byte] =
        Array.concat(
          s.offsetGenerator.getClass.getName.hashBytes,
          s.offsetGenerator.size.hashBytes,
          s.offsetGenerator.center.hashBytes,
          s.seed.hashBytes
        )

    val offsetGenerator: Lens[Spiral, OffsetGenerator.Spiral] =
      GenLens[Spiral](_.offsetGenerator)

    val seed: Lens[Spiral, Long] =
      GenLens[Spiral](_.seed)

  val spiral: Prism[TelescopeConfigGenerator, Spiral] =
    GenPrism[TelescopeConfigGenerator, Spiral]

  case class Uniform(
    offsetGenerator: OffsetGenerator.Uniform
  ) extends TelescopeConfigGenerator derives Eq

  object Uniform:

    given HashBytes[Uniform] with
      def hashBytes(u: Uniform): Array[Byte] =
        Array.concat(
          u.offsetGenerator.cornerA.hashBytes,
          u.offsetGenerator.cornerB.hashBytes
        )

    val offsetGenerator: Lens[Uniform, OffsetGenerator.Uniform] =
      GenLens[Uniform](_.offsetGenerator)

  val uniform: Prism[TelescopeConfigGenerator, Uniform] =
    GenPrism[TelescopeConfigGenerator, Uniform]

  val uniformOffsetGenerator: Optional[TelescopeConfigGenerator, OffsetGenerator.Uniform] = uniform.andThen(Uniform.offsetGenerator)
  val cornerA: Optional[TelescopeConfigGenerator, Offset] = uniformOffsetGenerator.andThen(OffsetGenerator.Uniform.cornerA)
  val cornerB: Optional[TelescopeConfigGenerator, Offset] = uniformOffsetGenerator.andThen(OffsetGenerator.Uniform.cornerB)

  val size: Optional[TelescopeConfigGenerator, Angle] =
    Optional[TelescopeConfigGenerator, Angle] {
      case Random(OffsetGenerator.Random(z, _), _) => z.some
      case Spiral(OffsetGenerator.Spiral(z, _), _) => z.some
      case _                                       => none
    } { z => {
      case Random(OffsetGenerator.Random(_, c), s) => Random(OffsetGenerator.Random(z, c), s)
      case Spiral(OffsetGenerator.Spiral(_, c), s) => Spiral(OffsetGenerator.Spiral(z, c), s)
      case tcg                                     => tcg
    }}

  val center: Optional[TelescopeConfigGenerator, Offset] =
    Optional[TelescopeConfigGenerator, Offset] {
      case Random(OffsetGenerator.Random(_, c), _) => c.some
      case Spiral(OffsetGenerator.Spiral(_, c), _) => c.some
      case _                                       => none
    } { c => {
      case Random(OffsetGenerator.Random(z, _), s) => Random(OffsetGenerator.Random(z, c), s)
      case Spiral(OffsetGenerator.Spiral(z, _), s) => Spiral(OffsetGenerator.Spiral(z, c), s)
      case tcg                                     => tcg
    }}

  val seed: Optional[TelescopeConfigGenerator, Long] =
    Optional[TelescopeConfigGenerator, Long] {
      case Random(OffsetGenerator.Random(_, _), s) => s.some
      case Spiral(OffsetGenerator.Spiral(_, _), s) => s.some
      case _                                       => none
    } { s => {
      case Random(OffsetGenerator.Random(z, c), _) => Random(OffsetGenerator.Random(z, c), s)
      case Spiral(OffsetGenerator.Spiral(z, c), _) => Spiral(OffsetGenerator.Spiral(z, c), s)
      case tcg                                     => tcg
    }}

  given HashBytes[TelescopeConfigGenerator] with
    def hashBytes(g: TelescopeConfigGenerator): Array[Byte] =
      g match
        case NoGenerator       => Array.empty
        case e @ Enumerated(_) => e.hashBytes
        case r @ Random(_, _)  => r.hashBytes
        case s @ Spiral(_, _)  => s.hashBytes
        case u @ Uniform(_)    => u.hashBytes

  given Eq[TelescopeConfigGenerator] with
    def eqv(x: TelescopeConfigGenerator, y: TelescopeConfigGenerator): Boolean =
      (x, y) match
        case (NoGenerator,       NoGenerator)       => true
        case (a @ Enumerated(_), b @ Enumerated(_)) => a === b
        case (a @ Random(_, _),  b @ Random(_, _))  => a === b
        case (a @ Spiral(_, _),  b @ Spiral(_, _))  => a === b
        case (a @ Uniform(_),    b @ Uniform(_))    => a === b
        case _                                      => false