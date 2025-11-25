// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Monad
import cats.data.NonEmptyList
import cats.effect.Async
import cats.effect.Concurrent
import cats.effect.std.Random
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.StepGuideState
import lucuma.core.geom.OffsetGenerator
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.model.Observation
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.odb.data.OffsetGeneratorRole
import lucuma.odb.graphql.input.OffsetGeneratorInput
import lucuma.odb.sequence.data.OffsetGeneratorType
import lucuma.odb.util.Codecs.angle_µas
import lucuma.odb.util.Codecs.guide_state
import lucuma.odb.util.Codecs.observation_id
import lucuma.odb.util.Codecs.offset
import lucuma.odb.util.Codecs.offset_generator_role
import lucuma.odb.util.Codecs.offset_generator_type
import lucuma.odb.util.Codecs.telescope_config
import skunk.*
import skunk.codec.numeric.int4
import skunk.implicits.*

import Services.Syntax.*

sealed trait OffsetGeneratorService[F[_]]:

  def insert(
    oids:  List[Observation.Id],
    input: OffsetGeneratorInput,
    role:  OffsetGeneratorRole,
  ): F[Unit]

  def generateGmosNorthImagingObject(
    oid:          Observation.Id,
    filterCounts: Map[GmosNorthFilter, (Int, Long)]
  ): F[Map[GmosNorthFilter, List[TelescopeConfig]]]

  def generateGmosNorthImagingSky(
    oid:   Observation.Id,
    count: Int,
    seed:  Long
  ): F[List[TelescopeConfig]]

  def generateGmosSouthImagingObject(
    oid:          Observation.Id,
    filterCounts: Map[GmosSouthFilter, (Int, Long)]
  ): F[Map[GmosSouthFilter, List[TelescopeConfig]]]

  def generateGmosSouthImagingSky(
    oid:   Observation.Id,
    count: Int,
    seed:  Long
  ): F[List[TelescopeConfig]]

object OffsetGeneratorService:

  def instantiate[F[_]: Async: Concurrent](using Services[F]): OffsetGeneratorService[F] =
    new OffsetGeneratorService[F]:

      override def insert(
        oids:  List[Observation.Id],
        input: OffsetGeneratorInput,
        role:  OffsetGeneratorRole,
      ): F[Unit] =
        val unit = Concurrent[F].unit

        def insertOffsetsIfEnumerated(
          which: NonEmptyList[Observation.Id]
        ): F[Unit] =
          input match
            case OffsetGeneratorInput.Enumerated(values) =>
              NonEmptyList.fromList(values).fold(unit): offs =>
                session.exec(Statements.insertEnumeratedOffsets(which, offs, role))
            case _                                       =>
              unit

        NonEmptyList
          .fromList(oids)
          .fold(unit): which =>
            session.exec(Statements.insert(which, input, role)) *>
              insertOffsetsIfEnumerated(which)

      override def generateGmosNorthImagingObject(
        o:  Observation.Id,
        fs: Map[GmosNorthFilter, (Int, Long)]
      ): F[Map[GmosNorthFilter, List[TelescopeConfig]]] =
        generateGmosImagingObject(
          o,
          fs,
          "t_gmos_north_imaging",
          "c_object_offset_generator_id"
        )

      override def generateGmosSouthImagingObject(
        o:  Observation.Id,
        fs: Map[GmosSouthFilter, (Int, Long)]
      ): F[Map[GmosSouthFilter, List[TelescopeConfig]]] =
        generateGmosImagingObject(
          o,
          fs,
          "t_gmos_south_imaging",
          "c_object_offset_generator_id"
        )

      private def generateGmosImagingObject[A](
        o:           Observation.Id,
        counts:      Map[A, (Int, Long)],
        modeTable:   String,
        inputColumn: String
      ): F[Map[A, List[TelescopeConfig]]] =
        for
          in <- selectInput(o, modeTable, inputColumn)
          os <- in.fold(List.empty.pure[F]): in =>
                  counts.toList.traverse:
                    case (a, (count, seed)) => generate(count, seed, in, StepGuideState.Enabled).tupleLeft(a)
        yield os.toMap

      override def generateGmosNorthImagingSky(
        oid:   Observation.Id,
        count: Int,
        seed:  Long
      ): F[List[TelescopeConfig]] =
        generateGmosImagingSky(
          oid,
          count,
          seed,
          "t_gmos_north_imaging",
          "c_sky_offset_generator_id"
        )

      override def generateGmosSouthImagingSky(
        oid:   Observation.Id,
        count: Int,
        seed:  Long
      ): F[List[TelescopeConfig]] =
        generateGmosImagingSky(
          oid,
          count,
          seed,
          "t_gmos_south_imaging",
          "c_sky_offset_generator_id"
        )

      private def generateGmosImagingSky[A](
        o:           Observation.Id,
        count:       Int,
        seed:        Long,
        modeTable:   String,
        inputColumn: String
      ): F[List[TelescopeConfig]] =
        for
          in <- selectInput(o, modeTable, inputColumn)
          os <- in.fold(List.empty.pure[F]): in =>
                  generate(count, seed, in, StepGuideState.Disabled)
        yield os


      private def selectInput(
        oid:       Observation.Id,
        modeTable: String,
        column:    String
      ): F[Option[OffsetGeneratorInput]] =
        for
          ps <- session.option(Statements.selectInput(modeTable, column))(oid)
          r  <- ps.traverse: (gen, a, b, s, c) =>
                  gen match
                    case OffsetGeneratorType.NoGenerator => OffsetGeneratorInput.NoGenerator.pure[F]
                    case OffsetGeneratorType.Enumerated  => selectEnumeratedOffsets(oid).map(OffsetGeneratorInput.Enumerated.apply)
                    case OffsetGeneratorType.Grid        => OffsetGeneratorInput.Grid(a, b).pure[F]
                    case OffsetGeneratorType.Random      => OffsetGeneratorInput.Random(s, c).pure[F]
                    case OffsetGeneratorType.Spiral      => OffsetGeneratorInput.Spiral(s, c).pure[F]
        yield r

      private def generate(count: Int, seed: Long, input: OffsetGeneratorInput, guideState: StepGuideState): F[List[TelescopeConfig]] =
        def withSeededRandom(fa: (Monad[F], Random[F]) ?=> F[NonEmptyList[Offset]]): F[List[TelescopeConfig]] =
          Random.scalaUtilRandomSeedLong(seed).flatMap: r =>
            given Random[F] = r
            fa.map(_.toList.map(o => TelescopeConfig(o, guideState)))

        PosInt.unapply(count).fold(List.empty[TelescopeConfig].pure[F]): posN =>
          input match
            case OffsetGeneratorInput.NoGenerator          =>
              List.empty[TelescopeConfig].pure[F]

            case OffsetGeneratorInput.Enumerated(lst)      =>
              lst.pure[F]

            case OffsetGeneratorInput.Grid(a, b)           =>
              val w = (a.p.toSignedDecimalArcseconds - b.p.toSignedDecimalArcseconds).abs
              val h = (a.q.toSignedDecimalArcseconds - b.q.toSignedDecimalArcseconds).abs
              val aspectRatio = w / h

              val cols0 = 1 max Math.sqrt(posN.value * aspectRatio.doubleValue).round.toInt
              val rows  = 1 max (posN.value.toDouble / cols0).ceil.toInt
              val cols  = (posN.value.toDouble / rows).ceil.toInt

              val stepP = if cols <= 2 then w else w / (cols - 1) // arcseconds
              val stepQ = if rows <= 2 then h else h / (rows - 1) // arcseconds

              val p0 = a.p.toSignedDecimalArcseconds min b.p.toSignedDecimalArcseconds
              val q0 = a.q.toSignedDecimalArcseconds max b.q.toSignedDecimalArcseconds
              val o  = Offset.signedDecimalArcseconds.reverseGet((p0, q0))

              val offsets =
                (0 until cols).toList.flatMap: c =>
                  (0 until rows).toList.map: r =>
                    o + Offset.signedDecimalArcseconds.reverseGet((stepP * c, stepQ * r))

              offsets.take(count).map(o => TelescopeConfig(o, guideState)).pure[F]

            case OffsetGeneratorInput.Random(size, center) =>
              withSeededRandom:
                OffsetGenerator.random(posN, size, center)

            case OffsetGeneratorInput.Spiral(size, center) =>
              withSeededRandom:
                OffsetGenerator.spiral(posN, size, center)

      private def selectEnumeratedOffsets(o: Observation.Id): F[List[TelescopeConfig]] =
        session.execute(Statements.SelectEnumeratedOffsets)(o)


  object Statements:

    def selectInput(
      table:  String,
      column: String
    ): Query[
      Observation.Id,
      (
        OffsetGeneratorType,
        Offset,
        Offset,
        Angle,
        Offset
      )
    ] =
      sql"""
        SELECT
          c_offset_generator_type,
          c_grid_corner_a_p,
          c_grid_corner_a_q,
          c_grid_corner_b_p,
          c_grid_corner_b_q,
          c_size,
          c_center_offset_p,
          c_center_offset_q
        FROM t_offset_generator g
        INNER JOIN #$table m ON m.#$column = g.c_offset_generator_id
        WHERE m.c_observation_id = $observation_id
      """.query(
        offset_generator_type *:
        offset                *:
        offset                *:
        angle_µas             *:
        offset
      )

    val SelectEnumeratedOffsets: Query[Observation.Id, TelescopeConfig] =
      sql"""
        SELECT
          c_offset_p,
          c_offset_q,
          c_guide_state
        FROM t_enumerated_offset o
        INNER JOIN t_offset_generator g ON g.c_offset_generator_id = o.c_offset_generator_id
        WHERE g.c_observation_id = $observation_id
        ORDER BY g.c_index
      """.query(telescope_config)

    def insert(
      which: NonEmptyList[Observation.Id],
      ogi:   OffsetGeneratorInput,
      role:  OffsetGeneratorRole
    ): AppliedFragment =
      def orZero(a: Option[Angle]): Angle = a.getOrElse(Angle.Angle0)

      val values =
        which.map: oid =>
          sql"""(
            $observation_id,
            $offset_generator_role,
            $offset_generator_type,
            $angle_µas,
            $angle_µas,
            $angle_µas,
            $angle_µas,
            $angle_µas,
            $angle_µas,
            $angle_µas
          )"""(
            oid,
            role,
            ogi.offsetGeneratorType,
            orZero(OffsetGeneratorInput.cornerA.getOption(ogi).map(_.p.toAngle)),
            orZero(OffsetGeneratorInput.cornerA.getOption(ogi).map(_.q.toAngle)),
            orZero(OffsetGeneratorInput.cornerB.getOption(ogi).map(_.p.toAngle)),
            orZero(OffsetGeneratorInput.cornerB.getOption(ogi).map(_.q.toAngle)),
            orZero(OffsetGeneratorInput.size.getOption(ogi)),
            orZero(OffsetGeneratorInput.center.getOption(ogi).map(_.p.toAngle)),
            orZero(OffsetGeneratorInput.center.getOption(ogi).map(_.q.toAngle))
          )

      void"""
        INSERT INTO t_offset_generator (
          c_observation_id,
          c_role,
          c_type,
          c_grid_corner_a_p,
          c_grid_corner_a_q,
          c_grid_corner_b_p,
          c_grid_corner_b_q,
          c_size,
          c_center_offset_p,
          c_center_offset_q
        ) VALUES
      """ |+| values.intercalate(void", ")

    def insertEnumeratedOffsets(
      which: NonEmptyList[Observation.Id],
      offs:  NonEmptyList[TelescopeConfig],
      role:  OffsetGeneratorRole
    ): AppliedFragment =
      val values =
        which.flatMap: oid =>
          offs.zipWithIndex.map: (off, idx) =>
            sql"""(
              $observation_id,
              $offset_generator_role,
              $int4,
              $angle_µas,
              $angle_µas,
              $guide_state
            )"""(
              oid,
              role,
              idx,
              off.offset.p.toAngle,
              off.offset.q.toAngle,
              off.guiding
            )

      void"""
        INSERT INTO t_enumerated_offset (
          c_observation_id,
          c_role,
          c_index,
          c_offset_p,
          c_offset_q,
          c_guide_state
        ) VALUES
      """ |+| values.intercalate(void", ")
/*
    def cloneEnumeratedOffsets(
      originalId: Observation.Id,
      newId:      Observation.Id
    ) =
      void"""
        INSERT INTO t_enumerated_offset
      """
*/