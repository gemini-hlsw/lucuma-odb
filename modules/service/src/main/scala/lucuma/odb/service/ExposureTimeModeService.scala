// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.option.*
import grackle.Problem
import grackle.Result
import grackle.ResultT
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.odb.data.ExposureTimeModeId
import lucuma.odb.data.ExposureTimeModeRole
import lucuma.odb.data.ExposureTimeModeType
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.service.Services.Syntax.*
import lucuma.odb.syntax.exposureTimeMode.*
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.implicits.*

sealed trait ExposureTimeModeService[F[_]]:

  def select(
    oids: NonEmptyList[Observation.Id],
    role: ExposureTimeModeRole
  )(using Transaction[F]): F[Map[Observation.Id, NonEmptyList[ExposureTimeMode]]]

  def selectRequirement(
    oids: NonEmptyList[Observation.Id]
  )(using Transaction[F]): F[Map[Observation.Id, ExposureTimeMode]]

  def insertOne(
    oid:  Observation.Id,
    role: ExposureTimeModeRole,
    etm:  ExposureTimeMode
  )(using Transaction[F]): F[ExposureTimeModeId]

  def insertMany(
    oids: NonEmptyList[Observation.Id],
    role: ExposureTimeModeRole,
    etm:  ExposureTimeMode
  )(using Transaction[F]): F[Map[Observation.Id, ExposureTimeModeId]]

  def deleteMany(
    oids: NonEmptyList[Observation.Id],
    role: ExposureTimeModeRole*
  )(using Transaction[F]): F[Unit]

  def updateOne(
    eid:    ExposureTimeModeId,
    update: ExposureTimeMode
  )(using Transaction[F]): F[Unit]

  def updateMany(
    oids:   NonEmptyList[Observation.Id],
    role:   ExposureTimeModeRole,
    update: ExposureTimeMode
  )(using Transaction[F]): F[Unit]

  def clone(
    originalId: Observation.Id,
    newId:      Observation.Id
  )(using Transaction[F]): F[Unit]

  def insertForSingleScienceEtm(
    observingModeName:   String,
    explicitAcquisition: Option[ExposureTimeMode],
    explicitScience:     Option[ExposureTimeMode],
    newRequirement:      Option[ExposureTimeMode],
    which:               List[Observation.Id]
  )(using Transaction[F]): F[Result[Unit]]

object ExposureTimeModeService:

  def instantiate[F[_]: Concurrent](using Services[F]): ExposureTimeModeService[F] =
    new ExposureTimeModeService[F]:

      override def select(
        oids: NonEmptyList[Observation.Id],
        role: ExposureTimeModeRole
      )(using Transaction[F]): F[Map[Observation.Id, NonEmptyList[ExposureTimeMode]]] =
        val af = Statements.Select(oids, role)
        session.prepareR(af.fragment.query(observation_id *: exposure_time_mode)).use: pq =>
          pq.stream(af.argument, chunkSize = 1024)
            .compile
            .toList
            .map: lst =>
              lst.groupMap(_._1)(_._2)
                 .view
                 .mapValues(NonEmptyList.fromListUnsafe)
                 .toMap

      override def selectRequirement(
        oids: NonEmptyList[Observation.Id]
      )(using Transaction[F]): F[Map[Observation.Id, ExposureTimeMode]] =
        select(oids, ExposureTimeModeRole.Requirement)
          .map(_.view.mapValues(_.head).toMap)

      override def insertOne(
        oid:  Observation.Id,
        role: ExposureTimeModeRole,
        etm:  ExposureTimeMode
      )(using Transaction[F]): F[ExposureTimeModeId] =
        session.unique(Statements.Insert)(oid, role, etm)

      override def insertMany(
        oids: NonEmptyList[Observation.Id],
        role: ExposureTimeModeRole,
        etm:  ExposureTimeMode
      )(using Transaction[F]): F[Map[Observation.Id, ExposureTimeModeId]] =
        val af = Statements.InsertMany(oids, role, etm)
        session.prepareR(af.fragment.query(observation_id *: exposure_time_mode_id)).use: pq =>
          pq.stream(af.argument, chunkSize = 1024)
            .compile
            .toList
            .map(_.toMap)

      override def deleteMany(
        oids:  NonEmptyList[Observation.Id],
        roles: ExposureTimeModeRole*
      )(using Transaction[F]): F[Unit] =
        session.exec(Statements.delete(oids, roles.toList))

      override def updateOne(
        eid:    ExposureTimeModeId,
        update: ExposureTimeMode
      )(using Transaction[F]): F[Unit] =
        session.execute(Statements.UpdateExposureTimeMode)(eid, update).void

      override def updateMany(
        oids:   NonEmptyList[Observation.Id],
        role:   ExposureTimeModeRole,
        update: ExposureTimeMode
      )(using Transaction[F]): F[Unit] =
        for
          _ <- session.exec(Statements.updateWherePresent(oids, role, update))
          _ <- session.exec(Statements.insertWhereNotPresent(oids, role, update))
        yield ()

      override def clone(
        originalOid: Observation.Id,
        newOid:      Observation.Id
      )(using Transaction[F]): F[Unit] =
        session.execute(Statements.Clone)(originalOid, newOid).void

      private def forSingleScienceObservingModes(
        observingModeName:   String,
        explicitAcquisition: Option[ExposureTimeMode],
        explicitScience:     Option[ExposureTimeMode],
        newRequirement:      Option[ExposureTimeMode],
        curRequirements:     Map[Observation.Id, ExposureTimeMode],
        which:               List[Observation.Id]
      ): Result[Map[Observation.Id, (ExposureTimeMode, ExposureTimeMode)]] =
        val sci =
          which
            .fproduct: oid =>
              explicitScience orElse newRequirement orElse curRequirements.get(oid)
            .collect:
              case (oid, Some(etm)) => oid -> etm
            .toMap

        val missing = which.toSet -- sci.keySet

        def success: Map[Observation.Id, (ExposureTimeMode, ExposureTimeMode)] =
          sci.fproductLeft: etm =>
            explicitAcquisition.getOrElse(ExposureTimeMode.forAcquisition(etm.at))

        def error: Problem =
          val msg  = s"${observingModeName} requires a science exposure time mode."
          OdbError.InvalidArgument(msg.some).asProblem

        Result.fromOption(Option.when(missing.isEmpty)(success), error)

      override def insertForSingleScienceEtm(
        observingModeName:   String,
        explicitAcquisition: Option[ExposureTimeMode],
        explicitScience:     Option[ExposureTimeMode],
        newRequirement:      Option[ExposureTimeMode],
        which:               List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        def insert(etms: List[(Observation.Id, ExposureTimeMode)], role: ExposureTimeModeRole): F[Unit] =
          etms.groupMap(_._2)(_._1).toList.traverse_ { (etm, oids) =>
            services.exposureTimeModeService.insertMany(NonEmptyList.fromListUnsafe(oids), role, etm)
          }

        NonEmptyList
          .fromList(which)
          .fold(Result.unit.pure[F]): nel =>
            (for
              c <- ResultT.liftF(selectRequirement(nel))
              m <- ResultT.fromResult(forSingleScienceObservingModes(observingModeName, explicitAcquisition, explicitScience, newRequirement, c, which))
              a  = m.toList.map { case (oid, (acq, _)) => oid -> acq }
              s  = m.toList.map { case (oid, (_, sci)) => oid -> sci }
              _ <- ResultT.liftF(insert(a, ExposureTimeModeRole.Acquisition))
              _ <- ResultT.liftF(insert(s, ExposureTimeModeRole.Science))
            yield ()).value

  object Statements:

    def Select(
      oids: NonEmptyList[Observation.Id],
      role: ExposureTimeModeRole,
    ): AppliedFragment =
      sql"""
        SELECT
          c_observation_id,
          c_exposure_time_mode,
          c_signal_to_noise_at,
          c_signal_to_noise,
          c_exposure_time,
          c_exposure_count
        FROM t_exposure_time_mode
        WHERE c_observation_id IN ${observation_id.list(oids.length).values}
        AND c_role = $exposure_time_mode_role
      """.apply(oids.toList, role)

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

    def InsertMany(
      oids: NonEmptyList[Observation.Id],
      role: ExposureTimeModeRole,
      etm:  ExposureTimeMode
    ): AppliedFragment =
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
          c_observation_id,
          $exposure_time_mode_role,
          $exposure_time_mode_type,
          ${signal_to_noise.opt},
          $wavelength_pm,
          ${time_span.opt},
          ${int4_pos.opt}
        FROM unnest(ARRAY[${observation_id.list(oids.length)}]) AS c_observation_id
        RETURNING
          c_observation_id,
          c_exposure_time_mode_id
      """.apply(
            role,
            etm.modeType,
            etm.signalToNoise,
            etm.at,
            etm.exposureTime,
            etm.exposureCount,
            oids.toList
          )

    def delete(
      oids:  NonEmptyList[Observation.Id],
      roles: List[ExposureTimeModeRole]
    ): AppliedFragment =
      sql"""
        DELETE FROM t_exposure_time_mode
              WHERE c_observation_id IN ${observation_id.list(oids.length).values}
      """.apply(oids.toList) |+| (
        if roles.isEmpty then void""
        else sql"AND c_role IN ${exposure_time_mode_role.list(roles.length).values}".apply(roles)
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
