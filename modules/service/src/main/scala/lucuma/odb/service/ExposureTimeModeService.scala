// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.MonadCancelThrow
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.math.SignalToNoise
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.core.util.TimeSpan
import lucuma.odb.data.ExposureTimeModeId
import lucuma.odb.data.ExposureTimeModeRole
import lucuma.odb.data.ExposureTimeModeType
import lucuma.odb.service.Services.Syntax.*
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.implicits.*

sealed trait ExposureTimeModeService[F[_]]:

  def insertExposureTimeMode(
    oid:  Observation.Id,
    role: ExposureTimeModeRole,
    etm:  ExposureTimeMode
  )(using Transaction[F]): F[ExposureTimeModeId]

  def deleteExposureTimeModes(
    oids: NonEmptyList[Observation.Id],
    role: Option[ExposureTimeModeRole]
  )(using Transaction[F]): F[Unit]

  def updateExposureTimeMode(
    eid:    ExposureTimeModeId,
    update: ExposureTimeMode
  )(using Transaction[F]): F[Unit]

  def updateExposureTimeModes(
    oids:   NonEmptyList[Observation.Id],
    role:   ExposureTimeModeRole,
    update: ExposureTimeMode
  )(using Transaction[F]): F[Unit]

  def cloneExposureTimeModes(
    originalId: Observation.Id,
    newId:      Observation.Id
  )(using Transaction[F]): F[Unit]

object ExposureTimeModeService:

  def instantiate[F[_]: MonadCancelThrow](using Services[F]): ExposureTimeModeService[F] =
    new ExposureTimeModeService[F]:

      override def insertExposureTimeMode(
        oid:  Observation.Id,
        role: ExposureTimeModeRole,
        etm:  ExposureTimeMode
      )(using Transaction[F]): F[ExposureTimeModeId] =
        session.unique(Statements.Insert)(oid, role, etm)

      override def deleteExposureTimeModes(
        oids: NonEmptyList[Observation.Id],
        role: Option[ExposureTimeModeRole]
      )(using Transaction[F]): F[Unit] =
        session.exec(Statements.delete(oids, role))

      override def updateExposureTimeMode(
        eid:    ExposureTimeModeId,
        update: ExposureTimeMode
      )(using Transaction[F]): F[Unit] =
        session.execute(Statements.UpdateExposureTimeMode)(eid, update).void

      override def updateExposureTimeModes(
        oids:   NonEmptyList[Observation.Id],
        role:   ExposureTimeModeRole,
        update: ExposureTimeMode
      )(using Transaction[F]): F[Unit] =
        for
          _ <- session.exec(Statements.updateWherePresent(oids, role, update))
          _ <- session.exec(Statements.insertWhereNotPresent(oids, role, update))
        yield ()

      override def cloneExposureTimeModes(
        originalOid: Observation.Id,
        newOid:      Observation.Id
      )(using Transaction[F]): F[Unit] =
        session.execute(Statements.Clone)(originalOid, newOid).void

  object Statements:

    extension (etm: ExposureTimeMode)
      def modeType: ExposureTimeModeType =
        etm match
          case ExposureTimeMode.SignalToNoiseMode(_, _)   => ExposureTimeModeType.SignalToNoiseMode
          case ExposureTimeMode.TimeAndCountMode(_, _, _) => ExposureTimeModeType.TimeAndCountMode

      def signalToNoise: Option[SignalToNoise] =
        ExposureTimeMode
          .signalToNoise
          .andThen(ExposureTimeMode.SignalToNoiseMode.value)
          .getOption(etm)

      def exposureTime: Option[TimeSpan] =
        ExposureTimeMode
          .timeAndCount
          .andThen(ExposureTimeMode.TimeAndCountMode.time)
          .getOption(etm)

      def exposureCount: Option[PosInt] =
        ExposureTimeMode
          .timeAndCount
          .andThen(ExposureTimeMode.TimeAndCountMode.count)
          .getOption(etm)

    val Insert: Query[(Observation.Id, ExposureTimeModeRole, ExposureTimeMode), ExposureTimeModeId] =
      sql"""
        INSERT INTO t_exposure_time_mode (
          c_observation_id,
          c_role,
          c_exposure_time_mode,
          c_signal_to_noise,
          c_signal_to_noise_at,
          c_exposure_time,
          c_exposure_count
        )
        SELECT
          $observation_id,
          $exposure_time_mode_role,
          $exposure_time_mode_type,
          ${signal_to_noise.opt},
          $wavelength_pm,
          ${time_span.opt},
          ${int4_pos.opt}
        RETURNING
          c_exposure_time_mode_id
      """.query(exposure_time_mode_id)
         .contramap: (oid, role, etm) =>
           (
             oid,
             role,
             etm.modeType,
             etm.signalToNoise,
             etm.at,
             etm.exposureTime,
             etm.exposureCount
           )

    def delete(
      oids: NonEmptyList[Observation.Id],
      role: Option[ExposureTimeModeRole]
    ): AppliedFragment =
      sql"""
        DELETE FROM t_exposure_time_mode
              WHERE c_observation_id IN ${observation_id.list(oids.length).values}
      """.apply(oids.toList) |+|
      role.fold(void"")(
        sql"""
          AND c_role = $exposure_time_mode_role
        """
      )

    val UpdateExposureTimeMode: Command[(ExposureTimeModeId, ExposureTimeMode)] =
      sql"""
        UPDATE t_exposure_time_mode
        SET c_exposure_time_mode = $exposure_time_mode_type,
            c_signal_to_noise    = ${signal_to_noise.opt},
            c_signal_to_noise_at = $wavelength_pm,
            c_exposure_time      = ${time_span.opt},
            c_exposure_count     = ${int4_pos.opt}
        WHERE
            c_exposure_time_mode_id = $exposure_time_mode_id
      """
        .command
        .contramap: (id, etm) =>
          (
            etm.modeType,
            etm.signalToNoise,
            etm.at,
            etm.exposureTime,
            etm.exposureCount,
            id
          )

    def updateWherePresent(
      oids:   NonEmptyList[Observation.Id],
      role:   ExposureTimeModeRole,
      update: ExposureTimeMode
    ): AppliedFragment =
      sql"""
        UPDATE t_exposure_time_mode
        SET c_exposure_time_mode = $exposure_time_mode_type,
            c_signal_to_noise    = ${signal_to_noise.opt},
            c_signal_to_noise_at = $wavelength_pm,
            c_exposure_time      = ${time_span.opt},
            c_exposure_count     = ${int4_pos.opt}
        WHERE c_observation_id IN ${observation_id.list(oids.length).values}
          AND c_role = $exposure_time_mode_role
      """.apply(
        update.modeType,
        update.signalToNoise,
        update.at,
        update.exposureTime,
        update.exposureCount,
        oids.toList,
        role
      )

    def insertWhereNotPresent(
      oids:   NonEmptyList[Observation.Id],
      role:   ExposureTimeModeRole,
      update: ExposureTimeMode
    ): AppliedFragment =
      sql"""
        WITH obs_ids AS (
          SELECT
            c_observation_id
          FROM unnest(ARRAY[${observation_id.list(oids.length)}]) AS t(c_observation_id)
          WHERE NOT EXISTS (
            SELECT 1
            FROM t_exposure_time_mode e
            WHERE e.c_observation_id = t.c_observation_id
              AND e.c_role = $exposure_time_mode_role
          )
        )
        INSERT INTO t_exposure_time_mode (
          c_observation_id,
          c_role,
          c_exposure_time_mode,
          c_signal_to_noise,
          c_signal_to_noise_at,
          c_exposure_time,
          c_exposure_count
        )
        SELECT
          c_observation_id,
          $exposure_time_mode_role,
          $exposure_time_mode_type,
          ${signal_to_noise.opt},
          $wavelength_pm,
          ${time_span.opt},
          ${int4_pos.opt}
        FROM
          obs_ids
      """.apply(
        oids.toList,
        role,
        role,
        update.modeType,
        update.signalToNoise,
        update.at,
        update.exposureTime,
        update.exposureCount
      )

    // (Original, New)
    val Clone: Command[(Observation.Id, Observation.Id)] =
      sql"""
        INSERT INTO t_exposure_time_mode (
          c_observation_id,
          c_role,
          c_exposure_time_mode,
          c_signal_to_noise,
          c_signal_to_noise_at,
          c_exposure_time,
          c_exposure_count
        )
        SELECT
          $observation_id,
          c_role,
          c_exposure_time_mode,
          c_signal_to_noise,
          c_signal_to_noise_at,
          c_exposure_time,
          c_exposure_count
        FROM t_exposure_time_mode
        WHERE c_observation_id = $observation_id
      """.command
         .contramap: (origOid, newOid) =>
           (newOid, origOid)

    val SelectLinks: Query[Observation.Id, (ExposureTimeModeId, ExposureTimeModeRole)] =
      sql"""
        SELECT
          c_exposure_time_mode_id,
          c_role
        FROM t_exposure_time_mode_link
        WHERE c_observation_id = $observation_id
      """.query(exposure_time_mode_id *: exposure_time_mode_role)
