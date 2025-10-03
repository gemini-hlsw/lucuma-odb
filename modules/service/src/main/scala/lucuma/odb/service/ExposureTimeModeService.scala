// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.MonadCancelThrow
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
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
    etm: ExposureTimeMode
  )(using Transaction[F]): F[ExposureTimeModeId]

  def updateExposureTimeMode(
    eid:    ExposureTimeModeId,
    update: ExposureTimeMode
  )(using Transaction[F]): F[Unit]

  def insertExposureTimeModeLink(
    oid:  Observation.Id,
    etm:  ExposureTimeMode,
    role: ExposureTimeModeRole
  )(using Transaction[F]): F[Unit]

  def deleteExposureTimeModeLinks(
    oids: NonEmptyList[Observation.Id],
    role: ExposureTimeModeRole
  )(using Transaction[F]): F[Unit]

  def updateExposureTimeModeLinks(
    oids: NonEmptyList[Observation.Id],
    etm:  ExposureTimeMode,
    role: ExposureTimeModeRole
  )(using Transaction[F]): F[Unit]

  def cloneExposureTimeModeLinks(
    originalId: Observation.Id,
    newId:      Observation.Id
  )(using Transaction[F]): F[Unit]

object ExposureTimeModeService:

  def instantiate[F[_]: MonadCancelThrow](using Services[F]): ExposureTimeModeService[F] =
    new ExposureTimeModeService[F]:

      override def insertExposureTimeMode(
        etm: ExposureTimeMode
      )(using Transaction[F]): F[ExposureTimeModeId] =
        session.unique(Statements.InsertExposureTimeMode)(etm)

      override def updateExposureTimeMode(
        eid: ExposureTimeModeId,
        etm: ExposureTimeMode
      )(using Transaction[F]): F[Unit] =
        session.execute(Statements.UpdateExposureTimeMode)(eid, etm).void

      override def insertExposureTimeModeLink(
        oid:  Observation.Id,
        etm:  ExposureTimeMode,
        role: ExposureTimeModeRole
      )(using Transaction[F]): F[Unit] =
        for
          e <- insertExposureTimeMode(etm)
          _ <- session.execute(Statements.InsertExposureTimeModeLink)(oid, e, role).void
        yield ()

      override def deleteExposureTimeModeLinks(
        oids: NonEmptyList[Observation.Id],
        role: ExposureTimeModeRole
      )(using Transaction[F]): F[Unit] =
        session.exec(Statements.deleteLinks(oids, role))

      override def updateExposureTimeModeLinks(
        oids: NonEmptyList[Observation.Id],
        etm:  ExposureTimeMode,
        role: ExposureTimeModeRole
      )(using Transaction[F]): F[Unit] =
        for
          _ <- session.exec(Statements.updateLinksWherePresent(oids, etm, role))
          _ <- session.exec(Statements.insertLinksWhereNotPresent(oids, etm, role))
        yield ()

      override def cloneExposureTimeModeLinks(
        originalOid: Observation.Id,
        newOid:      Observation.Id
      )(using Transaction[F]): F[Unit] =
        for
          ls <- session.execute(Statements.SelectLinks)(originalOid)
          _  <- ls.traverse_ { (originalEid, role) =>
                  session.unique(Statements.CloneExposureTimeMode)(originalEid).flatMap: newEid =>
                    session.execute(Statements.InsertExposureTimeModeLink)(newOid, newEid, role)
                }
        yield ()

  object Statements:

/*
    private val signal_to_noise_mode: Codec[ExposureTimeMode.SignalToNoiseMode] =
      (signal_to_noise *: wavelength_pm).to[ExposureTimeMode.SignalToNoiseMode]

    private val time_and_count_mode: Codec[ExposureTimeMode.TimeAndCountMode] =
      (time_span *: int4_pos *: wavelength_pm).to[ExposureTimeMode.TimeAndCountMode]

    private val exposure_time_mode: Codec[ExposureTimeMode] =
      (signal_to_noise_mode.opt *: time_and_count_mode.opt)
        .eimap {
          case (Some(_),  Some(_))  |
               (None,     None)     => "Exactly one of signal-to-noise or time-and-count mode must be specified.".asLeft
          case (Some(sn), None)     => sn.asRight.widen[ExposureTimeMode]
          case (None,     Some(tc)) => tc.asRight.widen[ExposureTimeMode]
        } { etm => (
            ExposureTimeMode.signalToNoise.getOption(etm),
            ExposureTimeMode.timeAndCount.getOption(etm)
          )
        }
*/

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

    val InsertExposureTimeMode: Query[ExposureTimeMode, ExposureTimeModeId] =
      sql"""
        INSERT INTO t_exposure_time_mode (
          c_exposure_time_mode,
          c_signal_to_noise,
          c_signal_to_noise_at,
          c_exposure_time,
          c_exposure_count
        )
        SELECT
          $exposure_time_mode_type,
          ${signal_to_noise.opt},
          $wavelength_pm,
          ${time_span.opt},
          ${int4_pos.opt}
        RETURNING
          c_exposure_time_mode_id
      """.query(exposure_time_mode_id)
         .contramap: etm =>
           (
             etm.modeType,
             etm.signalToNoise,
             etm.at,
             etm.exposureTime,
             etm.exposureCount
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


    val InsertExposureTimeModeLink: Command[(Observation.Id, ExposureTimeModeId, ExposureTimeModeRole)] =
      sql"""
        INSERT INTO t_exposure_time_mode_link (
          c_observation_id,
          c_exposure_time_mode_id,
          c_role
        )
        SELECT
          $observation_id,
          $exposure_time_mode_id,
          $exposure_time_mode_role
      """.command

    def updateLinksWherePresent(
      oids: NonEmptyList[Observation.Id],
      etm:  ExposureTimeMode,
      role: ExposureTimeModeRole
    ): AppliedFragment =
      sql"""
        UPDATE t_exposure_time_mode m
        SET c_exposure_time_mode = $exposure_time_mode_type,
            c_signal_to_noise    = ${signal_to_noise.opt},
            c_signal_to_noise_at = $wavelength_pm,
            c_exposure_time      = ${time_span.opt},
            c_exposure_count     = ${int4_pos.opt}
        FROM t_exposure_time_mode_link k
        WHERE k.c_observation_id IN ${observation_id.list(oids.length).values}
          AND m.c_exposure_time_mode_id = k.c_exposure_time_mode_id
          AND k.c_role = $exposure_time_mode_role
      """.apply(
        etm.modeType,
        etm.signalToNoise,
        etm.at,
        etm.exposureTime,
        etm.exposureCount,
        oids.toList,
        role
      )

    def insertLinksWhereNotPresent(
      oids: NonEmptyList[Observation.Id],
      etm:  ExposureTimeMode,
      role: ExposureTimeModeRole
    ): AppliedFragment =
      sql"""
        WITH obs_ids AS (
          SELECT
            c_observation_id,
            ROW_NUMBER() OVER () AS rn
          FROM unnest(ARRAY[${observation_id.list(oids.length)}]) WITH ORDINALITY AS t(c_observation_id)
          WHERE NOT EXISTS (
            SELECT 1
            FROM t_exposure_time_mode_link k
            WHERE k.c_observation_id = t.c_observation_id
          )
        ),
        exposure_time_mode_ids AS (
          INSERT INTO t_exposure_time_mode (
            c_exposure_time_mode,
            c_signal_to_noise,
            c_signal_to_noise_at,
            c_exposure_time,
            c_exposure_count
          )
          SELECT
            $exposure_time_mode_type,
            ${signal_to_noise.opt},
            $wavelength_pm,
            ${time_span.opt},
            ${int4_pos.opt}
          FROM obs_ids
          RETURNING c_exposure_time_mode_id
        ),
        numbered_exposure_time_mode_ids AS (
          SELECT
            c_exposure_time_mode_id,
            ROW_NUMBER() OVER () AS rn
          FROM exposure_time_mode_ids
        )
        INSERT INTO t_exposure_time_mode_link (
          c_observation_id,
          c_exposure_time_mode_id,
          c_role
        )
        SELECT
          o.c_observation_id,
          i.c_exposure_time_mode_id,
          $exposure_time_mode_role
        FROM obs_ids o
        JOIN numbered_exposure_time_mode_ids i USING (rn);
      """.apply(
        oids.toList,
        etm.modeType,
        etm.signalToNoise,
        etm.at,
        etm.exposureTime,
        etm.exposureCount,
        role
      )

    def deleteLinks(
      oids: NonEmptyList[Observation.Id],
      role: ExposureTimeModeRole
    ): AppliedFragment =
      sql"""
        DELETE FROM t_exposure_time_mode_link
              WHERE c_observation_id IN ${observation_id.list(oids.length).values}
                AND c_role = $exposure_time_mode_role
      """.apply(oids.toList, role)

    val CloneExposureTimeMode: Query[ExposureTimeModeId, ExposureTimeModeId] =
      sql"""
        INSERT INTO t_exposure_time_mode (
          c_exposure_time_mode,
          c_signal_to_noise,
          c_signal_to_noise_at,
          c_exposure_time,
          c_exposure_count
        )
        SELECT
          c_exposure_time_mode,
          c_signal_to_noise,
          c_signal_to_noise_at,
          c_exposure_time,
          c_exposure_count
        FROM t_exposure_time_mode
        WHERE c_exposure_time_mode_id = $exposure_time_mode_id
        RETURNING c_exposure_time_mode_id
      """.query(exposure_time_mode_id)

    val SelectLinks: Query[Observation.Id, (ExposureTimeModeId, ExposureTimeModeRole)] =
      sql"""
        SELECT
          c_exposure_time_mode_id,
          c_role
        FROM t_exposure_time_mode_link
        WHERE c_observation_id = $observation_id
      """.query(exposure_time_mode_id *: exposure_time_mode_role)
