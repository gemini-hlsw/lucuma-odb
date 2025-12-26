// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import lucuma.core.enums.StepGuideState
import lucuma.core.enums.TelescopeConfigGeneratorType
import lucuma.core.geom.OffsetGenerator
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.model.Observation
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.odb.data.TelescopeConfigGeneratorRole
import lucuma.odb.sequence.data.TelescopeConfigGenerator
import lucuma.odb.util.Codecs.angle_µas
import lucuma.odb.util.Codecs.guide_state
import lucuma.odb.util.Codecs.observation_id
import lucuma.odb.util.Codecs.offset
import lucuma.odb.util.Codecs.offset_generator_role
import lucuma.odb.util.Codecs.telescope_config
import lucuma.odb.util.Codecs.telescope_config_generator_type
import skunk.*
import skunk.codec.numeric.int4
import skunk.codec.numeric.int8
import skunk.implicits.*

import Services.Syntax.*

sealed trait TelescopeConfigGeneratorService[F[_]]:

  def select(
    oids: NonEmptyList[Observation.Id],
    role: TelescopeConfigGeneratorRole
  ): F[Map[Observation.Id, TelescopeConfigGenerator]]

  def selectWithSeed(
    oids: NonEmptyList[Observation.Id],
    role: TelescopeConfigGeneratorRole
  ): F[Map[Observation.Id, (TelescopeConfigGenerator, Long)]]

  def insert(
    oids:  NonEmptyList[Observation.Id],
    input: TelescopeConfigGenerator,
    role:  TelescopeConfigGeneratorRole,
  ): F[Unit]

  def delete(
    oids: NonEmptyList[Observation.Id],
    role: TelescopeConfigGeneratorRole
  ): F[Unit]

  def replace(
    oids:  NonEmptyList[Observation.Id],
    input: Option[TelescopeConfigGenerator],
    role:  TelescopeConfigGeneratorRole
  ): F[Unit]

  def clone(
    originalId: Observation.Id,
    newId:      Observation.Id
  ): F[Unit]

object TelescopeConfigGeneratorService:

  def instantiate[F[_]: Concurrent](using Services[F]): TelescopeConfigGeneratorService[F] =
    new TelescopeConfigGeneratorService[F]:

      override def select(
        oids: NonEmptyList[Observation.Id],
        role: TelescopeConfigGeneratorRole
      ): F[Map[Observation.Id, TelescopeConfigGenerator]] =
        selectWithSeed(oids, role).map(_.view.mapValues(_._1).toMap)

      override def selectWithSeed(
        oids: NonEmptyList[Observation.Id],
        role: TelescopeConfigGeneratorRole
      ): F[Map[Observation.Id, (TelescopeConfigGenerator, Long)]] =

        val generatorData: F[Map[Observation.Id, (Either[Unit, TelescopeConfigGenerator], Long)]] =
          val af = Statements.select(oids, role)
          session.prepareR(af.fragment.query(observation_id *: Statements.offset_generator *: int8)).use: pq =>
            pq.stream(af.argument, chunkSize = 1024)
              .compile
              .toList
              .map(_.map((oid, e, seed) => oid -> (e, seed)).toMap)

        val enumeratedOffsets: F[Map[Observation.Id, List[TelescopeConfig]]] =
          val af = Statements.selectEnumeratedOffsets(oids, role)
          session.prepareR(af.fragment.query(observation_id *: telescope_config)).use: pq =>
            pq.stream(af.argument, chunkSize = 1024)
              .groupAdjacentBy(_._1)
              .map: (oid, chunk) =>
                oid -> chunk.toList.map(_._2)
              .compile
              .toList
              .map(_.toMap)

        for
          g <- generatorData
          e <- enumeratedOffsets
        yield
          g.view
           .map:
             case (oid, (Left(()), s))   =>
               val os = e.get(oid).flatMap(NonEmptyList.fromList)
               oid -> (os.fold(TelescopeConfigGenerator.NoGenerator)(TelescopeConfigGenerator.Enumerated.apply), s)
             case (oid, (Right(gen), s)) =>
               oid -> (gen, s)
           .toMap

      override def insert(
        oids:  NonEmptyList[Observation.Id],
        input: TelescopeConfigGenerator,
        role:  TelescopeConfigGeneratorRole,
      ): F[Unit] =
        val unit = Concurrent[F].unit

        val insertOffsetsIfEnumerated: F[Unit] =
          input match
            case TelescopeConfigGenerator.Enumerated(offs) =>
              session.exec(Statements.insertEnumeratedOffsets(oids, offs, role))
            case _                                     =>
              unit

        session.exec(Statements.insert(oids, input, role)) *> insertOffsetsIfEnumerated

      override def delete(
        oids: NonEmptyList[Observation.Id],
        role: TelescopeConfigGeneratorRole
      ): F[Unit] =
        session.exec(Statements.deleteOffsetGenerator(oids, role)) // cascades to enumerated offsets

      override def replace(
        oids:  NonEmptyList[Observation.Id],
        input: Option[TelescopeConfigGenerator],
        role:  TelescopeConfigGeneratorRole
      ): F[Unit] =
        delete(oids, role) *>
        input.fold(Concurrent[F].unit): in =>
          insert(oids, in, role)

      override def clone(
        originalId: Observation.Id,
        newId:      Observation.Id
      ): F[Unit] =
        session.execute(Statements.CloneOffsetGenerator)(newId, originalId) *>
        session.execute(Statements.CloneEnumeratedOffsets)(newId, originalId).void

  object Statements:

    val offset_generator: Codec[Either[Unit, TelescopeConfigGenerator]] =
      (
        telescope_config_generator_type *:
        angle_µas             *:  // size
        offset                *:  // center
        offset                *:  // cornerA
        offset                    // cornerB
      ).imap[Either[Unit, TelescopeConfigGenerator]] { (genType, s, c, a, b) =>
          genType match
            case TelescopeConfigGeneratorType.NoGenerator => TelescopeConfigGenerator.NoGenerator.asRight
            case TelescopeConfigGeneratorType.Enumerated  => ().asLeft
            case TelescopeConfigGeneratorType.Uniform     => TelescopeConfigGenerator.FromOffsetGenerator(OffsetGenerator.Uniform(a, b)).asRight
            case TelescopeConfigGeneratorType.Random      => TelescopeConfigGenerator.FromOffsetGenerator(OffsetGenerator.Random(s, c)).asRight
            case TelescopeConfigGeneratorType.Spiral      => TelescopeConfigGenerator.FromOffsetGenerator(OffsetGenerator.Spiral(s, c)).asRight
      } {
        case Right(TelescopeConfigGenerator.NoGenerator)    => (TelescopeConfigGeneratorType.NoGenerator, Angle.Angle0, Offset.Zero, Offset.Zero, Offset.Zero)
        case Right(TelescopeConfigGenerator.FromOffsetGenerator(OffsetGenerator.Random(s, c)))   => (TelescopeConfigGeneratorType.Random,      s,            c,           Offset.Zero, Offset.Zero)
        case Right(TelescopeConfigGenerator.FromOffsetGenerator(OffsetGenerator.Spiral(s, c)))   => (TelescopeConfigGeneratorType.Spiral,      s,            c,           Offset.Zero, Offset.Zero)
        case Right(TelescopeConfigGenerator.FromOffsetGenerator(OffsetGenerator.Uniform(a, b)))  => (TelescopeConfigGeneratorType.Uniform,     Angle.Angle0, Offset.Zero, a,           b)
        case Left(())                              => (TelescopeConfigGeneratorType.Enumerated,  Angle.Angle0, Offset.Zero, Offset.Zero, Offset.Zero)
        case Right(TelescopeConfigGenerator.Enumerated(os)) => (TelescopeConfigGeneratorType.Enumerated,  Angle.Angle0, Offset.Zero, Offset.Zero, Offset.Zero)
      }

    def select(
      oids: NonEmptyList[Observation.Id],
      role: TelescopeConfigGeneratorRole
    ): AppliedFragment =
      void"""
        SELECT
          c_observation_id,
          c_type,
          c_size,
          c_center_offset_p,
          c_center_offset_q,
          c_uniform_corner_a_p,
          c_uniform_corner_a_q,
          c_uniform_corner_b_p,
          c_uniform_corner_b_q,
          c_seed
        FROM t_offset_generator
        WHERE
      """ |+| observationIdIn(oids) |+| sql" AND c_role = $offset_generator_role"(role)

    def selectEnumeratedOffsets(
      oids: NonEmptyList[Observation.Id],
      role: TelescopeConfigGeneratorRole
    ): AppliedFragment =
      sql"""
        SELECT
          c_observation_id,
          c_offset_p,
          c_offset_q,
          c_guide_state
        FROM t_enumerated_offset
        WHERE
         c_role = $offset_generator_role AND
       """.apply(role) |+| observationIdIn(oids) |+| void" ORDER BY c_observation_id, c_index"

    def insert(
      which: NonEmptyList[Observation.Id],
      ogi:   TelescopeConfigGenerator,
      role:  TelescopeConfigGeneratorRole
    ): AppliedFragment =
      def orZero(a: Option[Angle]): Angle = a.getOrElse(Angle.Angle0)

      val values =
        which.map: oid =>
          sql"""(
            $observation_id,
            $offset_generator_role,
            $telescope_config_generator_type,
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
            orZero(TelescopeConfigGenerator.cornerA.getOption(ogi).map(_.p.toAngle)),
            orZero(TelescopeConfigGenerator.cornerA.getOption(ogi).map(_.q.toAngle)),
            orZero(TelescopeConfigGenerator.cornerB.getOption(ogi).map(_.p.toAngle)),
            orZero(TelescopeConfigGenerator.cornerB.getOption(ogi).map(_.q.toAngle)),
            orZero(TelescopeConfigGenerator.size.getOption(ogi)),
            orZero(TelescopeConfigGenerator.center.getOption(ogi).map(_.p.toAngle)),
            orZero(TelescopeConfigGenerator.center.getOption(ogi).map(_.q.toAngle))
          )

      void"""
        INSERT INTO t_offset_generator (
          c_observation_id,
          c_role,
          c_type,
          c_uniform_corner_a_p,
          c_uniform_corner_a_q,
          c_uniform_corner_b_p,
          c_uniform_corner_b_q,
          c_size,
          c_center_offset_p,
          c_center_offset_q
        ) VALUES
      """ |+| values.intercalate(void", ")

    def insertEnumeratedOffsets(
      which: NonEmptyList[Observation.Id],
      offs:  NonEmptyList[TelescopeConfig],
      role:  TelescopeConfigGeneratorRole
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

    def deleteOffsetGenerator(
      which: NonEmptyList[Observation.Id],
      role:  TelescopeConfigGeneratorRole
    ): AppliedFragment =
      sql"""
        DELETE FROM t_offset_generator
        WHERE
          c_role = $offset_generator_role AND
          c_observation_id IN ${observation_id.list(which.length).values}
      """.apply(role, which.toList)

    // TODO: The random generator seed, is not cloned.  Should it be?
    // If so, the cloned observation would have the same "random" offsets.
    val CloneOffsetGenerator: Command[(Observation.Id, Observation.Id)] =
      sql"""
        INSERT INTO t_offset_generator (
          c_observation_id,
          c_role,
          c_type,
          c_uniform_corner_a_p,
          c_uniform_corner_a_q,
          c_uniform_corner_b_p,
          c_uniform_corner_b_q,
          c_size,
          c_center_offset_p,
          c_center_offset_q
        )
        SELECT
          $observation_id,
          c_role,
          c_type,
          c_uniform_corner_a_p,
          c_uniform_corner_a_q,
          c_uniform_corner_b_p,
          c_uniform_corner_b_q,
          c_size,
          c_center_offset_p,
          c_center_offset_q
        FROM t_offset_generator
        WHERE c_observation_id = $observation_id
      """.command

    // New, Old
    val CloneEnumeratedOffsets: Command[(Observation.Id, Observation.Id)] =
      sql"""
        INSERT INTO t_enumerated_offset (
          c_observation_id,
          c_role,
          c_index,
          c_offset_p,
          c_offset_q,
          c_guide_state
        )
        SELECT
          $observation_id,
          c_role,
          c_index,
          c_offset_p,
          c_offset_q,
          c_guide_state
        FROM t_enumerated_offset
        WHERE c_observation_id = $observation_id
      """.command
