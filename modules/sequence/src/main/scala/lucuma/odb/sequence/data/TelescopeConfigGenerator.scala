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
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.StepGuideState
import lucuma.core.geom.OffsetGenerator
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.odb.sequence.syntax.hash.*
import lucuma.odb.sequence.util.HashBytes
import monocle.Iso
import monocle.Optional
import monocle.Prism
import monocle.macros.GenPrism

sealed trait TelescopeConfigGenerator:

  import TelescopeConfigGenerator.*

  def offsetGeneratorType: TelescopeConfigGeneratorType =
    this match
      case NoGenerator   => TelescopeConfigGeneratorType.NoGenerator
      case Enumerated(_) => TelescopeConfigGeneratorType.Enumerated
      case FromOffsetGenerator(OffsetGenerator.Random(_, _))  => TelescopeConfigGeneratorType.Random
      case FromOffsetGenerator(OffsetGenerator.Spiral(_, _))  => TelescopeConfigGeneratorType.Spiral
      case FromOffsetGenerator(OffsetGenerator.Uniform(_, _)) => TelescopeConfigGeneratorType.Uniform

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
        case TelescopeConfigGenerator.NoGenerator          =>
          List.empty[TelescopeConfig].pure[F]

        case TelescopeConfigGenerator.Enumerated(lst)      =>
          // Enumerated positions come with an explicit guide state so the
          // default is ignored.
          LazyList.continually(lst.toList).flatten.take(count.value).toList.pure[F]

        case TelescopeConfigGenerator.FromOffsetGenerator(og) =>
          withSeededRandom:
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

  case class FromOffsetGenerator(
    offsetGenerator: OffsetGenerator
  ) extends TelescopeConfigGenerator derives Eq

  object FromOffsetGenerator:
    val offsetGenerator: Iso[FromOffsetGenerator, OffsetGenerator] =
      Iso[FromOffsetGenerator, OffsetGenerator](_.offsetGenerator)(FromOffsetGenerator(_))

    given HashBytes[FromOffsetGenerator] with
      def hashBytes(fog: FromOffsetGenerator): Array[Byte] =
        fog.offsetGenerator match
          case OffsetGenerator.Random(size, center) =>
            Array.concat(fog.offsetGenerator.getClass.getName.hashBytes, size.hashBytes, center.hashBytes)
          case OffsetGenerator.Spiral(size, center) =>
            Array.concat(fog.offsetGenerator.getClass.getName.hashBytes, size.hashBytes, center.hashBytes)
          case OffsetGenerator.Uniform(cornerA, cornerB)       =>
            Array.concat(cornerA.hashBytes, cornerB.hashBytes)

  val fromOffsetGenerator: Prism[TelescopeConfigGenerator, FromOffsetGenerator] =
    GenPrism[TelescopeConfigGenerator, FromOffsetGenerator]

  val offsetGenerator: Optional[TelescopeConfigGenerator, OffsetGenerator] =
    fromOffsetGenerator.andThen(FromOffsetGenerator.offsetGenerator)

  val uniform: Optional[TelescopeConfigGenerator, OffsetGenerator.Uniform]= offsetGenerator.andThen(OffsetGenerator.uniform)
  val cornerA: Optional[TelescopeConfigGenerator, Offset] = uniform.andThen(OffsetGenerator.Uniform.cornerA)
  val cornerB: Optional[TelescopeConfigGenerator, Offset] = uniform.andThen(OffsetGenerator.Uniform.cornerB)

  val size: Optional[TelescopeConfigGenerator, Angle] = offsetGenerator.andThen(OffsetGenerator.size)

  val center: Optional[TelescopeConfigGenerator, Offset] = offsetGenerator.andThen(OffsetGenerator.center)

  given HashBytes[TelescopeConfigGenerator] with
    def hashBytes(g: TelescopeConfigGenerator): Array[Byte] =
      g match
        case NoGenerator       => Array.empty
        case e @ Enumerated(_) => e.hashBytes
        case fog @ FromOffsetGenerator(_) => fog.hashBytes

  given Eq[TelescopeConfigGenerator] with
    def eqv(x: TelescopeConfigGenerator, y: TelescopeConfigGenerator): Boolean =
      (x, y) match
        case (NoGenerator, NoGenerator)             => true
        case (a @ Enumerated(_), b @ Enumerated(_)) => a === b
        case (a @ FromOffsetGenerator(_), b @ FromOffsetGenerator(_)) => a === b
        case _                                      => false