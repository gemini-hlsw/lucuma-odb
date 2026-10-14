// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.implicits.*
import grackle.Result
import lucuma.core.model.Observation
import lucuma.odb.graphql.input.DaytimeCalibrationInput
import lucuma.odb.sequence.daytimecal.Config
import lucuma.odb.service.Services.Syntax.session
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.data.Completion
import skunk.syntax.all.*

trait DaytimeCalibrationService[F[_]]:

  def select(
    which: List[Observation.Id]
  ): F[Map[Observation.Id, Config]]

  def insert(
    input: DaytimeCalibrationInput.Create,
    which: List[Observation.Id]
  )(using Transaction[F]): F[Result[Unit]]

  def delete(
    which: List[Observation.Id]
  )(using Transaction[F]): F[Unit]

  def update(
    SET: DaytimeCalibrationInput.Edit,
    which: List[Observation.Id]
  )(using Transaction[F]): F[Unit]

  def clone(
    originalId: Observation.Id,
    newId: Observation.Id
  )(using Transaction[F]): F[Unit]

object DaytimeCalibrationService:

  def instantiate[F[_]: Concurrent](using Services[F]): DaytimeCalibrationService[F] =
    new DaytimeCalibrationService[F]:

      override def select(
        which: List[Observation.Id]
      ): F[Map[Observation.Id, Config]] =
        NonEmptyList.fromList(which) match
          case None      => Map.empty.pure
          case Some(nel) =>
            session.prepareR(Statements.select(nel)).use: pq =>
              pq.stream(nel, 1024).compile.toList.map(_.toMap)

      override def insert(
        input: DaytimeCalibrationInput.Create,
        which: List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        session.prepareR(Statements.Insert).use: ps =>
          which
            .traverse: oid =>
              ps.execute(oid, input).map:
                case Completion.Insert(1) => Result.unit
                case other => Result.failure(s"Expected single insert, got $other.")
            .map(_.sequenceVoid)

      override def delete(
        which: List[Observation.Id]
      )(using Transaction[F]): F[Unit] =
        NonEmptyList.fromList(which) match
          case None      => ().pure
          case Some(nel) =>
            session.prepareR(Statements.delete(nel)).use: ps =>
              ps.execute(nel).void

      override def update(
        SET: DaytimeCalibrationInput.Edit,
        which: List[Observation.Id]
      )(using Transaction[F]): F[Unit] =
        (SET.instrument, NonEmptyList.fromList(which)).tupled.traverse: (inst, oids) =>
          val af = sql"""
            UPDATE t_daytime_calibration
            SET c_instrument = $instrument
            WHERE c_observation_id IN (${observation_id.nel(oids)})
          """(inst, oids)
          session.prepareR(af.fragment.command).use: ps =>
            ps.execute(af.argument)
        .void

      override def clone(
        originalId: Observation.Id,
        newId: Observation.Id
      )(using Transaction[F]): F[Unit] =
        select(List(originalId)).map(_.get(originalId)).flatMap:
          case None         =>
            Concurrent[F].raiseError(new RuntimeException(s"Clone: original observation $originalId has no daytime calibration config."))
          case Some(config) =>
            insert(config, List(newId)).flatMap: res =>
              res.toEither match
                case Left(Left(t))   => Concurrent[F].raiseError(t)
                case Left(Right(ps)) => Concurrent[F].raiseError(new RuntimeException(s"Clone: insert failed: ${ps.toList.mkString(", ")}"))
                case Right(a)        => a.pure

  private object Statements:

    val daytime_calibration_config: Codec[Config] =
      instrument.imap(Config.apply)(_.instrument)

    def select[A <: NonEmptyList[Observation.Id]](a: A): Query[a.type, (Observation.Id, Config)] =
      sql"""
        SELECT
          c_observation_id,
          c_instrument
        FROM
          t_daytime_calibration
        WHERE
          c_observation_id IN (${observation_id.nel(a)})
      """.query(observation_id *: daytime_calibration_config)

    val Insert: Command[(Observation.Id, DaytimeCalibrationInput.Create)] =
      sql"""
        INSERT INTO t_daytime_calibration (
          c_observation_id,
          c_instrument
        ) VALUES (
          $observation_id,
          $daytime_calibration_config
        )
      """.command

    def delete[A <: NonEmptyList[Observation.Id]](a: A): Command[a.type] =
      sql"""
        DELETE FROM t_daytime_calibration
        WHERE c_observation_id IN (${observation_id.nel(a)})
      """.command
