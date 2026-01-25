// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.TelluricCalibrationOrder
import lucuma.core.math.SignalToNoise
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.util.TimeSpan
import lucuma.odb.data.BlindOffsetType
import lucuma.odb.data.Existence
import lucuma.odb.data.ExposureTimeModeRole
import lucuma.odb.data.GroupTree
import lucuma.odb.data.Nullable
import lucuma.odb.data.PosAngleConstraintMode
import lucuma.odb.graphql.input.CreateGroupInput
import lucuma.odb.graphql.input.GroupPropertiesInput
import lucuma.odb.graphql.input.ObservationPropertiesInput
import lucuma.odb.graphql.input.PosAngleConstraintInput
import lucuma.odb.graphql.input.TargetEnvironmentInput
import lucuma.odb.graphql.mapping.AccessControl
import lucuma.odb.service.Services.SuperUserAccess
import lucuma.odb.util.Codecs.*
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.syntax.*
import skunk.*
import skunk.AppliedFragment
import skunk.Query
import skunk.Transaction
import skunk.syntax.all.*

trait PerScienceObservationCalibrationsService[F[_]]:

  def generateCalibrations(
    pid:        Program.Id,
    scienceObs: List[ObsExtract[CalibrationConfigSubset]],
    oid:        Observation.Id
  )(using Transaction[F], SuperUserAccess): F[(List[Observation.Id], List[Observation.Id])]

object PerScienceObservationCalibrationsService:
  def instantiate[F[_]: {Concurrent as F, Logger, Services as S}]: PerScienceObservationCalibrationsService[F] =
    new PerScienceObservationCalibrationsService[F] with CalibrationObservations with WorkflowStateQueries[F]:

      val groupService  = S.groupService
      val observationService = S.observationService
      val obsModeService = S.observingModeServices
      val telluricTargets = S.telluricTargetsService
      val obscalcService = S.obscalcService

      private val MultiTelluricThreshold: TimeSpan = TimeSpan.fromHours(1.5).get

      private def groupNameForObservation(
        config:          CalibrationConfigSubset,
        calibrationRole: CalibrationRole,
        oid:             Observation.Id
      ): NonEmptyString =
        NonEmptyString.unsafeFrom(s"${config.modeType.dbTag}/${calibrationRole.tag}/${oid.show}")

      private def findSystemGroupForObservation(
        tree: GroupTree,
        oid:  Observation.Id
      ): Option[Group.Id] =
        tree.collectGroups(
          oid,
          b => b.system && b.calibrationRoles.exists(_ == CalibrationRole.Telluric)
        )

      private def findParentGroupForObservation(
        tree: GroupTree,
        oid:  Observation.Id
      ): Option[Group.Id] =
        tree.collectGroups(oid, b => !b.system)

      private def newTelluricGroup(
        pid:           Program.Id,
        config:        CalibrationConfigSubset,
        oid:           Observation.Id,
        parentGroupId: Option[Group.Id],
        parentIndex:   Option[NonNegShort]
      )(using Transaction[F]): F[Group.Id] =
        groupService.createGroup(
          CreateGroupInput(
            programId = pid.some,
            proposalReference = none,
            programReference = none,
            SET = GroupPropertiesInput.Create(
              name = groupNameForObservation(config, CalibrationRole.Telluric, oid).some,
              description = none,
              minimumRequired = none,
              ordered = true,
              minimumInterval = none,
              maximumInterval = TimeSpan.Zero.some,
              parentGroupId = parentGroupId,
              parentGroupIndex = parentIndex,
              existence = Existence.Present
            ),
            initialContents = List(Right(oid))
          ),
          system = true,
          calibrationRoles = List(CalibrationRole.Telluric)
        ).orError

      private def findAllTelluricObservations(gid: Group.Id): F[List[Observation.Id]] =
        S.session
          .prepareR(Statements.selectTelluricObservations)
          .use(_.stream((gid, CalibrationRole.Telluric), 10).compile.toList)

      private def obsDuration(
        scienceOid: Observation.Id
      )(using Transaction[F]): F[Option[TimeSpan]] =
        obscalcService.selectExecutionDigest(scienceOid).map:
          _.flatMap(_.value.toOption)
            .map(d => d.science.timeEstimate.sum |+| d.science.timeEstimate.nonCharged)

      private def insertTelluricObservation(
        pid:             Program.Id,
        telluricGroupId: Group.Id,
        telluricIndex:   NonNegShort
      )(using Transaction[F], SuperUserAccess): F[Observation.Id] =
        // Minimal input to create the telluric obs
        val targetEnvironment = TargetEnvironmentInput.Create(
          explicitBase = none,
          asterism = none, // We resolve the target later
          useBlindOffset = false.some,
          blindOffsetTarget = none,
          blindOffsetType = BlindOffsetType.Manual
        )

        val obsInput = ObservationPropertiesInput.Create(
          subtitle = none,
          scienceBand = none,
          posAngleConstraint = PosAngleConstraintInput(
            mode = PosAngleConstraintMode.AverageParallactic.some,
            angle = none
          ).some,
          targetEnvironment = targetEnvironment.some,
          constraintSet = none,
          timingWindows = none,
          attachments = none,
          scienceRequirements = none,
          observingMode = none,
          existence = Existence.Present.some,
          group = telluricGroupId.some,
          groupIndex = telluricIndex.some,
          observerNotes = none
        )
        observationService
          .createObservation(
            AccessControl.unchecked(obsInput, pid, program_id),
            calibrationRole = CalibrationRole.Telluric.some
          ).orError

      private def createTelluricObs(
        pid:             Program.Id,
        scienceOid:      Observation.Id,
        telluricGroupId: Group.Id,
        telluricIndex:   NonNegShort,
        duration:        TimeSpan,
        order:           TelluricCalibrationOrder
      )(using Transaction[F], SuperUserAccess): F[Observation.Id] =
        for {
          telluricId <- insertTelluricObservation(pid, telluricGroupId, telluricIndex)
          _          <- telluricTargets.requestTelluricTarget(pid, telluricId, scienceOid, duration, order)
          _          <- syncConfiguration(scienceOid, telluricId)
        } yield telluricId

      private def createTelluricCalibrations(
        pid:             Program.Id,
        scienceOid:      Observation.Id,
        telluricGroupId: Group.Id
      )(using Transaction[F], SuperUserAccess): F[List[Observation.Id]] =
        def obsGroupIndex(scienceOid: Observation.Id): F[NonNegShort] =
          S.session
            .prepareR(Statements.selectScienceObservationIndex)
            .use(_.unique(scienceOid))

        for
          duration <- obsDuration(scienceOid)
          created  <- duration match
            case Some(d) if d > MultiTelluricThreshold =>
              // Over 1.5h: 1 telluric before and 1 after
              for {
                sciIdx <- obsGroupIndex(scienceOid)
                bIdx   = NonNegShort.unsafeFrom(sciIdx.value.toShort)
                c1     <- createTelluricObs(pid, scienceOid, telluricGroupId, bIdx, d, TelluricCalibrationOrder.Before)
                aftIdx = NonNegShort.unsafeFrom((sciIdx.value + 2).toShort)
                c2     <- createTelluricObs(pid, scienceOid, telluricGroupId, aftIdx, d, TelluricCalibrationOrder.After)
              } yield List(c1, c2)

            case Some(d) =>
              // Less than 1.5h: one telluric after science
              for {
                sciIdx <- obsGroupIndex(scienceOid)
                aftIdx = NonNegShort.unsafeFrom((sciIdx.value + 1).toShort)
                cal    <- createTelluricObs(pid, scienceOid, telluricGroupId, aftIdx, d, TelluricCalibrationOrder.After)
              } yield List(cal)

            case None =>
              // No duration available, skip
              List.empty[Observation.Id].pure[F]
        yield created

      private def readTelluricGroup(
        pid:  Program.Id,
        tree: GroupTree,
        obs:  ObsExtract[CalibrationConfigSubset]
      )(using Transaction[F]): F[Group.Id] =
        val obsIndexMap = tree.collectObservations(_ => true).flatMap((_, obs) => obs).toMap
        findSystemGroupForObservation(tree, obs.id)
          .fold(
            newTelluricGroup(
              pid,
              obs.data,
              obs.id,
              findParentGroupForObservation(tree, obs.id),
              obsIndexMap.get(obs.id)
            )
          )(gid => gid.pure[F])

      private def syncTelluricObservation(
        pid: Program.Id,
        obs: ObsExtract[CalibrationConfigSubset],
        gid: Group.Id
      )(using Transaction[F], SuperUserAccess): F[(List[Observation.Id], List[Observation.Id])] =
        for {
          existing      <- findAllTelluricObservations(gid)
          deletable     <- excludeFromDeletion(existing, identity)
          duration      <- obsDuration(obs.id)
          requiredCount  = duration match
                             case Some(d) if d > MultiTelluricThreshold => 2
                             case Some(_)                               => 1
                             case None                                  => 0
          // Delete/recreate if count changes
          (created, deleted) <- if (existing.size != requiredCount)
                                  for
                                    _ <- NonEmptyList.fromList(deletable)
                                          .traverse_(observationService.deleteCalibrationObservations)
                                    c <- createTelluricCalibrations(pid, obs.id, gid)
                                  yield (c, deletable)
                                else
                                  (List.empty, List.empty).pure[F]
          // sync configuration on all deletable tellurics
          allTellurics  <- findAllTelluricObservations(gid)
          toSync        <- excludeFromDeletion(allTellurics, identity)
          _             <- toSync.traverse_(tid => syncConfiguration(obs.id, tid))
        } yield (created, deleted)

      private def generateTelluricForScience(
        pid:  Program.Id,
        tree: GroupTree,
        obs:  ObsExtract[CalibrationConfigSubset]
      )(using Transaction[F], SuperUserAccess): F[(List[Observation.Id], List[Observation.Id])] =
        for
          gid    <- readTelluricGroup(pid, tree, obs)
          result <- syncTelluricObservation(pid, obs, gid)
        yield result

      private val MaxTelluricSN = SignalToNoise.fromInt(100).get

      // After cloning we have the same etm as science, but we need a different one for tellurics
      // https://app.shortcut.com/lucuma/story/6968/generate-telluric-standard-sequence
      private def createTelluricExposureTimeMode(
        scienceOid:  Observation.Id,
        telluricOid: Observation.Id
      )(using Transaction[F]): F[Unit] =
        def telluricEtm(etm: ExposureTimeMode): ExposureTimeMode.SignalToNoiseMode =
          val snValue = etm match
            case ExposureTimeMode.SignalToNoiseMode(sn, _) =>
              SignalToNoise.unsafeFromBigDecimalExact(
                (sn.toBigDecimal * 2).min(MaxTelluricSN.toBigDecimal)
              )
            case _ =>
              MaxTelluricSN
          ExposureTimeMode.SignalToNoiseMode(snValue, etm.at)

        for {
          allEtm     <- S.exposureTimeModeService
                           .select(List(scienceOid, telluricOid), ExposureTimeModeRole.Science)
                           .map(_.view.mapValues(_.head).toMap)
          scienceEtm  = allEtm.get(scienceOid)
          _          <- scienceEtm.traverse_ : etm =>
                          val calibEtm   = telluricEtm(etm)
                          val currentEtm = allEtm.get(telluricOid)

                          pprint.pprintln(s" $scienceOid $telluricOid")
                          pprint.pprintln(scienceEtm)
                          pprint.pprintln(telluricEtm)

                          (S.exposureTimeModeService.deleteMany(List(telluricOid), ExposureTimeModeRole.Science) *>
                            S.exposureTimeModeService.insertOne(telluricOid, ExposureTimeModeRole.Science, calibEtm))
                              .unlessA(currentEtm.contains(calibEtm))
        } yield ()

      private def syncConfiguration(
        sourceOid: Observation.Id,
        targetOid: Observation.Id
      )(using Transaction[F], SuperUserAccess): F[Unit] =

        def readObservingModes: F[List[(Observation.Id, Option[ObservingModeType])]] =
          val af = Statements.selectObservingModeTypes(NonEmptyList.of(sourceOid, targetOid))
          S.session
            .prepareR(af.fragment.query(observation_id *: observing_mode_type.opt))
            .use(_.stream(af.argument, 2).compile.toList)

        def extractModes(
          modes: List[(Observation.Id, Option[ObservingModeType])]
        ): F[(Option[ObservingModeType], Option[ObservingModeType])] =
          modes match
            case (sid, sourceMode) :: (tid, targetMode) :: Nil if (sid === sourceOid && tid === targetOid) =>
              (sourceMode, targetMode).pure[F]
            case (tid, targetMode) :: (sid, sourceMode) :: Nil if (sid === sourceOid && tid === targetOid) =>
              (sourceMode, targetMode).pure[F]
            case _ =>
              F.raiseError(new RuntimeException("Cannot read obs modes for source and target"))

        def syncObservationProperties: F[Unit] =
          S.session.executeCommand(Statements.syncObservationConfiguration(sourceOid, targetOid)).void

        def deleteOldTargetMode(tm: Option[ObservingModeType]): F[Unit] =
          tm.traverse(mode => obsModeService.delete(mode, List(targetOid))).void

        def deleteAllExposureTimeModes(sm: Option[ObservingModeType]): F[Unit] =
          sm.traverse(_ => S.exposureTimeModeService.deleteMany(
            List(targetOid),
            ExposureTimeModeRole.Requirement,
            ExposureTimeModeRole.Acquisition,
            ExposureTimeModeRole.Science
          )).void

        def updateTargetModeType(sm: Option[ObservingModeType]): F[Unit] =
          sm.traverse(mode =>
            S.session.execute(Statements.updateObservingModeType)(mode.some, mode.instrument.some, targetOid)
          ).void

        def cloneSourceMode(sm: Option[ObservingModeType]): F[Unit] =
          sm.traverse(mode => obsModeService.clone(mode, sourceOid, targetOid)).void

        for {
          modes    <- readObservingModes
          (sm, tm) <- extractModes(modes)
          _        <- syncObservationProperties
          _        <- deleteOldTargetMode(tm)
          _        <- deleteAllExposureTimeModes(sm)
          _        <- updateTargetModeType(sm)
          _        <- cloneSourceMode(sm)
          _        <- createTelluricExposureTimeMode(sourceOid, targetOid)
        } yield ()

      private def findTelluricGroupForObservation(
        oid: Observation.Id
      ): F[Option[Group.Id]] =
        S.session
          .prepareR(Statements.selectTelluricGroupForObservation)
          .use(_.option((oid, CalibrationRole.Telluric)))

      // Move the science observation out of the telluric group to its parent group
      private def moveScienceObservationOutOfGroup(
        telluricGroupId: Group.Id,
        scienceOid:      Observation.Id
      )(using Transaction[F], SuperUserAccess): F[Unit] =
        for
          parentInfo <- S.session
                          .prepareR(Statements.selectGroupParentInfo)
                          .use(_.unique(telluricGroupId))
          (parentGroupId, parentIndex) = parentInfo
          _ <- observationService.updateObservations(
                 Services.asSuperUser:
                   AccessControl.unchecked(
                     ObservationPropertiesInput.Edit.Empty.copy(
                       group      = parentGroupId.map(Nullable.NonNull(_)).getOrElse(Nullable.Null),
                       groupIndex = parentIndex.some
                     ),
                     List(scienceOid),
                     observation_id
                   )
               )
        yield ()

      private def cleanupOrphanedTelluricGroup(
        pid:        Program.Id,
        gid:        Group.Id,
        scienceOid: Observation.Id
      )(using Transaction[F], SuperUserAccess): F[(List[Observation.Id], List[Observation.Id])] =
        for
          // Query only telluric calibration observations in the group, not all observations
          telluricOidsInGroup <- S.session
                                   .prepareR(Statements.selectTelluricObservations)
                                   .use(_.stream((gid, CalibrationRole.Telluric), 1024).compile.toList)
          deletable           <- excludeFromDeletion(telluricOidsInGroup, identity)
          deleted             <- NonEmptyList.fromList(deletable) match
                                   case Some(nel) => observationService.deleteCalibrationObservations(nel).as(deletable)
                                   case None      => List.empty.pure[F]
          _                   <- (info"Deleted ${deleted.size} orphaned telluric observations for group $gid: $deleted").whenA(deleted.nonEmpty)
          // Only delete the group if ALL telluric observations were deleted
          allDeleted           = deleted.size == telluricOidsInGroup.size
          // Move the science observation out of the group before deleting the group
          _                   <- moveScienceObservationOutOfGroup(gid, scienceOid).whenA(allDeleted)
          _                   <- groupService.deleteSystemGroup(pid, gid).whenA(allDeleted)
          _                   <- (info"Deleted orphaned telluric group $gid").whenA(allDeleted)
          _                   <- (info"Telluric group $gid retained: ${telluricOidsInGroup.size - deleted.size} observations have visits").whenA(!allDeleted && deleted.nonEmpty)
        yield (List.empty, deleted)

      override def generateCalibrations(
        pid:        Program.Id,
        scienceObs: List[ObsExtract[CalibrationConfigSubset]],
        oid:        Observation.Id
      )(using Transaction[F], SuperUserAccess): F[(List[Observation.Id], List[Observation.Id])] =
        for
          activeScienceObs <- onlyDefinedAndReady(scienceObs, _.id)
          changedObs        = activeScienceObs.find(_.id === oid)
          _                <- info"Targeted calibration recalculation for observation $oid"
          _                <- S.session.execute(sql"set constraints all deferred".command)
          tree             <- groupService.selectGroups(pid)
          result           <- changedObs match
            case Some(obs) =>
              info"Observation $oid is active per-obs type, generating telluric" *>
                generateTelluricForScience(pid, tree, obs)
            case None =>
              for
                gidOpt  <- findTelluricGroupForObservation(oid)
                result  <- gidOpt match
                  case Some(gid) =>
                    info"Observation $oid no longer active, cleaning up telluric group $gid" *>
                      cleanupOrphanedTelluricGroup(pid, gid, oid)
                  case None =>
                    info"Observation $oid has no telluric group, skipping" *>
                      (List.empty[Observation.Id], List.empty[Observation.Id]).pure[F]
              yield result
        yield result

      object Statements:

        val selectTelluricGroupForObservation: Query[(Observation.Id, CalibrationRole), Group.Id] =
          sql"""
            SELECT g.c_group_id
            FROM   t_observation o
            JOIN   t_group g ON o.c_group_id = g.c_group_id
            WHERE  o.c_observation_id = $observation_id
              AND  g.c_system = true
              AND  $calibration_role = ANY(g.c_calibration_roles)
          """.query(group_id)

        val selectTelluricObservations: Query[(Group.Id, CalibrationRole), Observation.Id] =
          sql"""
            SELECT c_observation_id
            FROM   t_observation
            WHERE  c_group_id         = $group_id
              AND  c_calibration_role = $calibration_role
            ORDER BY c_group_index
          """.query(observation_id)

        def selectObservingModeTypes(
          oids: NonEmptyList[Observation.Id]
        ): AppliedFragment =
          void"""
            SELECT c_observation_id, c_observing_mode_type
            FROM   t_observation
            WHERE  c_observation_id IN (
          """ |+| oids.map(sql"$observation_id").intercalate(void", ") |+| void")"

        val updateObservingModeType: Command[(Option[ObservingModeType], Option[Instrument], Observation.Id)] =
          sql"""
            UPDATE t_observation
            SET    c_observing_mode_type = ${observing_mode_type.opt},
                   c_instrument          = ${instrument.opt}
            WHERE c_observation_id = $observation_id
          """.command

        val selectScienceObservationIndex: Query[Observation.Id, NonNegShort] =
          sql"""
            SELECT c_group_index
            FROM   t_observation
            WHERE  c_observation_id = $observation_id
          """.query(int2_nonneg)

        val selectGroupParentInfo: Query[Group.Id, (Option[Group.Id], NonNegShort)] =
          sql"""
            SELECT c_parent_id, c_parent_index
            FROM   t_group
            WHERE  c_group_id = $group_id
          """.query(group_id.opt *: int2_nonneg)

        def syncObservationConfiguration(
          sourceOid: Observation.Id,
          targetOid: Observation.Id
        ): AppliedFragment =
          sql"""
            UPDATE t_observation target
            SET
              c_cloud_extinction         = source.c_cloud_extinction,
              c_image_quality            = source.c_image_quality,
              c_sky_background           = source.c_sky_background,
              c_water_vapor              = source.c_water_vapor,
              c_air_mass_min             = source.c_air_mass_min,
              c_air_mass_max             = source.c_air_mass_max,
              c_hour_angle_min           = source.c_hour_angle_min,
              c_hour_angle_max           = source.c_hour_angle_max,
              c_spec_wavelength          = source.c_spec_wavelength,
              c_spec_resolution          = source.c_spec_resolution,
              c_spec_wavelength_coverage = source.c_spec_wavelength_coverage,
              c_spec_focal_plane         = source.c_spec_focal_plane,
              c_spec_focal_plane_angle   = source.c_spec_focal_plane_angle,
              c_spec_capability          = source.c_spec_capability,
              c_img_minimum_fov          = source.c_img_minimum_fov,
              c_img_narrow_filters       = source.c_img_narrow_filters,
              c_img_broad_filters        = source.c_img_broad_filters,
              c_img_combined_filters     = source.c_img_combined_filters
            FROM t_observation source
            WHERE source.c_observation_id = $observation_id
              AND target.c_observation_id = $observation_id
          """.apply(sourceOid, targetOid)
