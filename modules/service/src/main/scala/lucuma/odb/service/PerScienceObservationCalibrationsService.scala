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
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.TelluricCalibrationOrder
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.util.TimeSpan
import lucuma.odb.data.BlindOffsetType
import lucuma.odb.data.Existence
import lucuma.odb.data.ExposureTimeModeRole
import lucuma.odb.data.GroupTree
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
    scienceObs: List[ObsExtract[CalibrationConfigSubset]]
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

      private def telluricGroups(tree: GroupTree): Map[Group.Id, List[(Observation.Id, NonNegShort)]] =
        tree
          .collectObservations(b => b.system && b.calibrationRoles.contains(CalibrationRole.Telluric))
          .toMap

      private def observationsToMove(
        allObsInGroups: Map[Group.Id, List[(Observation.Id, NonNegShort)]],
        toUnlink:       Set[Observation.Id],
        groupLocations: Map[Group.Id, (Option[Group.Id], NonNegShort)],
        telluricObsIds: Set[Observation.Id]
      ): List[(Observation.Id, Option[Group.Id], Option[NonNegShort])] =
        allObsInGroups.toList.flatMap: (gid, obsWithIndices) =>
          // Only move science observations (exclude telluric calibrations)
          val toMove = obsWithIndices.filter: (o, _) =>
            toUnlink.exists(o === _) && !telluricObsIds.contains(o)
          val (parentGroupId, parentIndex) =
            groupLocations.get(gid)
              .map { case (pid, idx) => (pid, idx.some) }
              .getOrElse((none, none))
          toMove.map((oid, _) => (oid, parentGroupId, parentIndex))

      private def findTelluricObservation(gid: Group.Id): F[Option[Observation.Id]] =
        S.session
          .prepareR(Statements.selectTelluricObservation)
          .use(_.option((gid, CalibrationRole.Telluric)))

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

      private def deleteTelluricObservationsFromGroups(
        groupIds: List[Group.Id]
      )(using Transaction[F], SuperUserAccess): F[List[Observation.Id]] =
        for
          allTelluricOids <- groupIds.flatTraverse(gid => findTelluricObservation(gid).map(_.toList))
          // Filter ongoing/completed and with visits
          toDelete        <- excludeFromDeletion(allTelluricOids, identity)
          deleted         <- NonEmptyList.fromList(toDelete) match
                               case Some(nel) => observationService.deleteCalibrationObservations(nel).as(toDelete)
                               case None      => List.empty.pure[F]
        yield deleted

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

        for
          modes    <- readObservingModes
          (sm, tm) <- extractModes(modes)
          _        <- syncObservationProperties
          _        <- deleteOldTargetMode(tm)
          _        <- deleteAllExposureTimeModes(sm)
          _        <- updateTargetModeType(sm)
          _        <- cloneSourceMode(sm)
        yield ()

      override def generateCalibrations(
        pid:        Program.Id,
        scienceObs: List[ObsExtract[CalibrationConfigSubset]]
      )(using Transaction[F], SuperUserAccess): F[(List[Observation.Id], List[Observation.Id])] =
        for
          // only include observations that are Defined or Ready
          activeScienceObs     <- onlyDefinedAndReady(scienceObs, _.id)
          currentObsIds         = activeScienceObs.map(_.id).toSet
          _                    <- info"Recalculating per science calibrations for $pid"
          _                    <- debug"Program $pid has ${currentObsIds.size} science configurations"
          _                    <- S.session.execute(sql"set constraints all deferred".command)
          // Telluric groups with all observations
          allObsInGroups       <- groupService.selectGroups(pid, obsFilter = void"true").map(telluricGroups).map(_.toMap)
          // Collect telluric observation IDs from all groups
          telluricObsSet       <- allObsInGroups.keys.toList
                                   .flatTraverse(findAllTelluricObservations)
                                   .map(_.toSet)
          // Get all observations in groups excluding telluric calibrations
          allGroupObsIds        = allObsInGroups.values.flatten.map(_._1).filterNot(telluricObsSet.contains).toList
          // Find observations that are Ongoing or Completed - these should not be deleted
          ongoingOrCompletedIds <- filterWorkflowStateIn(allGroupObsIds, identity, List(ObservationWorkflowState.Ongoing, ObservationWorkflowState.Completed), ready = false).map(_.toSet)
          // Observations to remove from telluric groups (exclude active science obs AND Ongoing/Completed obs)
          toUnlink              = allObsInGroups.values.flatten
                                    .map(_._1)
                                    .filterNot(a =>
                                        currentObsIds.exists(_ === a) || ongoingOrCompletedIds.exists(_ === a))
                                    .toSet
          // Query group locations
          groupLocations        <- Statements.queryGroupLocations(allObsInGroups.keys.toList)
          // Collect all observations to move with their target locations (only science obs, not calibrations)
          _                     <- (info"Remove ${toUnlink.size} observations from their telluric groups on program $pid: $toUnlink").whenA(toUnlink.nonEmpty)
          toMove                = observationsToMove(allObsInGroups, toUnlink, groupLocations, telluricObsSet)
          // Move all observations in a single database call
          _                     <- (info"Move ${toMove.size} observations to telluric groups on program $pid: ${toMove.map(_._1)}").whenA(toMove.nonEmpty)
          _                     <- Statements.moveObservations(toMove)
          // Compute which groups are now empty (all observations are being unlinked)
          emptyGroupIds         = allObsInGroups.collect:
                                    case (gid, obsWithIndices) if obsWithIndices.forall((o, _) => toUnlink.exists(_ === o)) => gid
          // Delete telluric calibration observations from empty groups
          deleted               <- deleteTelluricObservationsFromGroups(emptyGroupIds.toList)
          _                     <- (info"Deleted ${deleted.size} telluric observations on program $pid: $deleted").whenA(deleted.nonEmpty)
          deletedSet            = deleted.toSet
          // delete groups where all tellurics are gone
          groupsToDelete        = emptyGroupIds.filter: gid =>
                                    allObsInGroups.get(gid).forall: obsWithIndices =>
                                      obsWithIndices
                                        .filter((oid, _) => telluricObsSet.contains(oid))
                                        .forall((oid, _) => deletedSet.contains(oid))
          // Delete empty telluric groups using deleteSystemGroup
          _                     <- (info"Remove ${groupsToDelete.size} empty telluric groups on program $pid: $groupsToDelete").whenA(groupsToDelete.nonEmpty)
          _                     <- groupsToDelete.toList.traverse_(gid => groupService.deleteSystemGroup(pid, gid))
          // Reload tree for group creation/lookup
          tree                  <- groupService.selectGroups(pid)
          // Create/sync telluric for each science obs
          results               <- activeScienceObs.traverse(obs => generateTelluricForScience(pid, tree, obs))
          added                 = results.flatMap(_._1)
          allDeleted            = deleted ++ results.flatMap(_._2)
          _                     <- (info"Added ${added.size} telluric observations on program $pid: $added").whenA(added.nonEmpty)
        yield (added, allDeleted)

      object Statements:

        val selectTelluricObservation: Query[(Group.Id, CalibrationRole), Observation.Id] =
          sql"""
            SELECT c_observation_id
            FROM   t_observation
            WHERE  c_group_id         = $group_id
              AND  c_calibration_role = $calibration_role
            LIMIT 1
          """.query(observation_id)

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

        def groupLocations(gids: List[Group.Id]): Query[gids.type, (Group.Id, Option[Group.Id], NonNegShort)] =
          sql"""
            select c_group_id, c_parent_id, c_parent_index
            from t_group
            where c_group_id in (${group_id.list(gids)})
          """.query(group_id *: group_id.opt *: int2_nonneg).contramap(_ => gids)

        // For each gruop return the parent groupid and the location on that gorup
        def queryGroupLocations(gids: List[Group.Id]): F[Map[Group.Id, (Option[Group.Id], NonNegShort)]] =
          gids match
            case Nil => Map.empty[Group.Id, (Option[Group.Id], NonNegShort)].pure[F]
            case _ =>
              S.session.prepareR(groupLocations(gids))
                .use(_.stream(gids, 1024).compile.toList)
                .map(_.map { case (gid, parentId, parentIndex) =>
                  gid -> ((parentId, parentIndex))
                }.toMap)

        def moveObservations(
          moves: List[(Observation.Id, Option[Group.Id], Option[NonNegShort])]
        ): F[Unit] =
          NonEmptyList.fromList(moves) match
            case None => ().pure[F]
            case Some(nel) =>
              val values = nel.map { case (oid, pgid, pidx) =>
                sql"(${observation_id}, ${group_id.opt}, ${int2_nonneg.opt})".apply(oid, pgid, pidx)
              }
              val valuesFragment = values.intercalate(void", ")
              val query =
                void"""
                  select group_move_observation(obs_id, parent_id, parent_idx)
                  from (values """ |+|
                  valuesFragment |+|
                  void""") as moves(obs_id, parent_id, parent_idx)
                """
              S.session.prepareR(query.fragment.query(void))
                .use(_.stream(query.argument, 1024).compile.drain)
