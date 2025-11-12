// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.traverse.*
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

/**
 * A service that facilitates working with the t_exposure_time_mode table.
 */
sealed trait ExposureTimeModeService[F[_]]:

  def select(
    oids: List[Observation.Id],
    role: ExposureTimeModeRole
  )(using Transaction[F]): F[Map[Observation.Id, NonEmptyList[ExposureTimeMode]]]

  def selectRequirement(
    oids: List[Observation.Id]
  )(using Transaction[F]): F[Map[Observation.Id, ExposureTimeMode]]

  def insertOne(
    oid:  Observation.Id,
    role: ExposureTimeModeRole,
    etm:  ExposureTimeMode
  )(using Transaction[F]): F[ExposureTimeModeId]

  def insertMany(
    oids: List[Observation.Id],
    role: ExposureTimeModeRole,
    etm:  ExposureTimeMode
  )(using Transaction[F]): F[List[(Observation.Id, ExposureTimeModeId)]]

  def deleteMany(
    oids: List[Observation.Id],
    role: ExposureTimeModeRole*
  )(using Transaction[F]): F[Unit]

  def updateOne(
    eid:    ExposureTimeModeId,
    update: ExposureTimeMode
  )(using Transaction[F]): F[Unit]

  def updateMany(
    oids:   List[Observation.Id],
    role:   ExposureTimeModeRole,
    update: ExposureTimeMode
  )(using Transaction[F]): F[Unit]

  /**
   * Clones all the exposure time modes associated with the 'originalId'
   * observation, returning the old exposure time mode id to new exposure time
   * mode id mapping.
   */
  def clone(
    originalId: Observation.Id,
    newId:      Observation.Id
  )(using Transaction[F]): F[List[(ExposureTimeModeId, ExposureTimeModeId)]]


  /**
   * Resolves the exposure time mode to use for acquisition and science given
   * any explicit values and the new science requirement value (if any).  This
   * method will look up the existing requirements exposure time mode for use
   * when explicit values are not provided.  If an exposure time mode cannot
   * be determined, an error result is returned.
   *
   * Note that multiple science ETMs can be resolved, each associated with an
   * arbitrary object.  This is used, for example, in GMOS imaging to associate
   * ETMs with filters.
   *
   * @return a map from observation to a tuple of (acquisition) exposure time
   *         mode and a NonEmptyList of science exposure time mdoes
   */
  def resolve[A](
    observingModeName:   String,
    explicitAcquisition: Option[ExposureTimeMode],
    explicitScience:     NonEmptyList[(A, Option[ExposureTimeMode])],
    newRequirement:      Option[ExposureTimeMode],
    which:               List[Observation.Id]
  )(using Transaction[F]): F[Result[Map[Observation.Id, (ExposureTimeMode, NonEmptyList[(A, ExposureTimeMode)])]]]

  /**
   * Inserts science exposure time modes and returns their ids.
   */
  def insertResolvedScienceOnly[A](
    resolved: Map[Observation.Id, NonEmptyList[(A, ExposureTimeMode)]]
  )(using Transaction[F]): F[Map[Observation.Id, NonEmptyList[(A, ExposureTimeModeId)]]]

  /**
   * Inserts acquisition and science exposure time modes and returns their ids.
   */
  def insertResolvedAcquisitionAndScience[A](
    resolved: Map[Observation.Id, (ExposureTimeMode, NonEmptyList[(A, ExposureTimeMode)])]
  )(using Transaction[F]): F[Map[Observation.Id, (ExposureTimeModeId, NonEmptyList[(A, ExposureTimeModeId)])]]

  /**
   * Computes and inserts the acquisition and science exposure time mode entries
   * for modes which have a single science exposure time mode.  Explicit values
   * may be provided.  If not, then if a new requirement ETM is being added or
   * updated, we base the science / acquisition ETMs on that.  If not, then we
   * base them on the existing requirement ETM (if any).  Otherwise, an error
   * result.
   *
   * This is a convenience for modes (like GMOS and F2 long slit) that have a
   * single exposure time mode for all of their science steps.
   *
   * @return map of (acquisition exposure time mode, science exposure time mode)
   */
  def insertOneWithDefaults(
    observingModeName:   String,
    explicitAcquisition: Option[ExposureTimeMode],
    explicitScience:     Option[ExposureTimeMode],
    newRequirement:      Option[ExposureTimeMode],
    which:               List[Observation.Id]
  )(using Transaction[F]): F[Result[Map[Observation.Id, (ExposureTimeModeId, ExposureTimeModeId)]]]

object ExposureTimeModeService:

  def instantiate[F[_]: Concurrent](using Services[F]): ExposureTimeModeService[F] =
    new ExposureTimeModeService[F]:

      override def select(
        oids: List[Observation.Id],
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
        oids: List[Observation.Id]
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
        oids: List[Observation.Id],
        role: ExposureTimeModeRole,
        etm:  ExposureTimeMode
      )(using Transaction[F]): F[List[(Observation.Id, ExposureTimeModeId)]] =
        val af = Statements.InsertMany(oids, role, etm)
        session.prepareR(af.fragment.query(observation_id *: exposure_time_mode_id)).use: pq =>
          pq.stream(af.argument, chunkSize = 1024)
            .compile
            .toList

      override def deleteMany(
        oids:  List[Observation.Id],
        roles: ExposureTimeModeRole*
      )(using Transaction[F]): F[Unit] =
        session.exec(Statements.delete(oids, roles.toList))

      override def updateOne(
        eid:    ExposureTimeModeId,
        update: ExposureTimeMode
      )(using Transaction[F]): F[Unit] =
        session.execute(Statements.UpdateExposureTimeMode)(eid, update).void

      override def updateMany(
        oids:   List[Observation.Id],
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
      )(using Transaction[F]): F[List[(ExposureTimeModeId, ExposureTimeModeId)]] =
        session.execute(Statements.Clone)(originalOid, newOid)

      override def insertOneWithDefaults(
        observingModeName:   String,
        explicitAcquisition: Option[ExposureTimeMode],
        explicitScience:     Option[ExposureTimeMode],
        newRequirement:      Option[ExposureTimeMode],
        which:               List[Observation.Id]
      )(using Transaction[F]): F[Result[Map[Observation.Id, (ExposureTimeModeId, ExposureTimeModeId)]]] =
        (for
          r <- ResultT(resolve[Unit](observingModeName, explicitAcquisition, NonEmptyList.one(((), explicitScience)), newRequirement, which))
          m <- ResultT.liftF(insertResolvedAcquisitionAndScience(r))
        yield m.view.mapValues((acq, sci) => (acq, sci.head._2)).toMap).value

      override def resolve[A](
        observingModeName:   String,
        explicitAcquisition: Option[ExposureTimeMode],
        explicitScience:     NonEmptyList[(A, Option[ExposureTimeMode])],
        newRequirement:      Option[ExposureTimeMode],
        which:               List[Observation.Id]
      )(using Transaction[F]): F[Result[Map[Observation.Id, (ExposureTimeMode, NonEmptyList[(A, ExposureTimeMode)])]]] =

        val science: F[Result[Map[Observation.Id, NonEmptyList[(A, ExposureTimeMode)]]]] =
          selectRequirement(which).map: curRequirements =>
            val sci =
              which
                .fproduct: oid =>
                  explicitScience.traverse: tup =>
                    tup.traverse: otm =>
                      otm orElse newRequirement orElse curRequirements.get(oid)
                .collect:
                  case (oid, Some(lst)) => oid -> lst
                .toMap

            val missing = which.toSet -- sci.keySet

            def error: Problem =
              val msg  = s"${observingModeName} requires a science exposure time mode."
              OdbError.InvalidArgument(msg.some).asProblem

            Result.fromOption(Option.when(missing.isEmpty)(sci), error)

        ResultT(science)
          .map: m =>
           // Add acquisition ETM.  When there are multiple science ETMs we
           // just pick the first for the `at`.
            m.fproductLeft: sci =>
              explicitAcquisition.getOrElse(ExposureTimeMode.forAcquisition(sci.head._2.at))
          .value

      override def insertResolvedScienceOnly[A](
        resolved: Map[Observation.Id, NonEmptyList[(A, ExposureTimeMode)]]
      )(using Transaction[F]): F[Map[Observation.Id, NonEmptyList[(A, ExposureTimeModeId)]]] =

        val groupedByMode: List[(ExposureTimeMode, List[(Observation.Id, A)])] =
          resolved
            .toList
            .flatMap: (oid, nel) =>
              nel.toList.map((a, mode) => (mode, (oid, a)))
            .groupMap(_._1)(_._2)  // Map[ExposureTimeMode, List[(Observation.Id, A)]]
            .toList

        groupedByMode
          .flatTraverse: (etm, lst) =>
            val (os, as) = lst.toList.unzip
            services
              .exposureTimeModeService
              .insertMany(os, ExposureTimeModeRole.Science, etm)
              .map: lst =>
                lst  // List[(Observation.Id, ExposureTimeModeId)]
                  .zip(as)
                  .map:
                    case ((o, eid), a) => (o, (a, eid))
                  .groupMap(_._1)(_._2)
                  .view
                  .mapValues(NonEmptyList.fromListUnsafe)
                  .toList
          .map(_.foldMap { case (oid, nel) => Map(oid -> nel) })

      override def insertResolvedAcquisitionAndScience[A](
        resolved: Map[Observation.Id, (ExposureTimeMode, NonEmptyList[(A, ExposureTimeMode)])]
      )(using Transaction[F]): F[Map[Observation.Id, (ExposureTimeModeId, NonEmptyList[(A, ExposureTimeModeId)])]] =

        def insertAcquisition(
          etms: List[(Observation.Id, ExposureTimeMode)]
        ): F[Map[Observation.Id, ExposureTimeModeId]] =
          etms
            .groupMap(_._2)(_._1)
            .toList
            .flatTraverse: (etm, oids) =>
              services
                .exposureTimeModeService
                .insertMany(oids, ExposureTimeModeRole.Acquisition, etm)
                .map(_.toList)
            .map(_.toMap)

        val a = resolved.toList.map { case (oid, (acq, _)) => oid -> acq }
        val s = resolved.view.mapValues { case (_, sci) => sci }.toMap

        for
          aʹ <- insertAcquisition(a)
          sʹ <- insertResolvedScienceOnly(s)
        yield aʹ.map((oid, acqEtm) => oid -> (acqEtm, sʹ(oid)))

  object Statements:

    def Select(
      oids: List[Observation.Id],
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
      oids: List[Observation.Id],
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
            oids
          )

    def delete(
      oids:  List[Observation.Id],
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
      oids:   List[Observation.Id],
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
        oids,
        role
      )

    def insertWhereNotPresent(
      oids:   List[Observation.Id],
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
        oids,
        role,
        role,
        update.modeType,
        update.signalToNoise,
        update.at,
        update.exposureTime,
        update.exposureCount
      )

    // (Original, New)
    val Clone: Query[(Observation.Id, Observation.Id), (ExposureTimeModeId, ExposureTimeModeId)] =
      sql"""
        SELECT
          e.old_exposure_time_mode_id,
          e.new_exposure_time_mode_id
        FROM clone_exposure_time_modes($observation_id, $observation_id) AS e
      """.query(exposure_time_mode_id *: exposure_time_mode_id)