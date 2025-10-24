// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import grackle.ResultT
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.TargetDisposition
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.RightAscension
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.Target
import lucuma.odb.Config
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
import lucuma.odb.graphql.input.SiderealInput
import lucuma.odb.graphql.input.TargetEnvironmentInput
import lucuma.odb.graphql.input.TargetPropertiesInput
import lucuma.odb.graphql.mapping.AccessControl
import lucuma.odb.service.Services.SuperUserAccess
import lucuma.odb.util.Codecs.*
import org.http4s.client.Client
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.syntax.*
import skunk.*
import skunk.Transaction
import skunk.implicits.*

import scala.collection.immutable.SortedMap

trait PerScienceObservationCalibrationsService[F[_]]:

  def generateCalibrations(
    pid: Program.Id,
    scienceObs: List[ObsExtract[CalibrationConfigSubset]]
  )(using Transaction[F], SuperUserAccess): F[(List[Observation.Id], List[Observation.Id])]

object PerScienceObservationCalibrationsService:
  def instantiate[F[_]: {Concurrent as F, Logger, Services as S}](
    emailConfig: Config.Email,
    httpClient:  Client[F]
  ): PerScienceObservationCalibrationsService[F] =
    new PerScienceObservationCalibrationsService[F] with CalibrationObservations:

      private def groupNameForObservation(
        config:          CalibrationConfigSubset,
        calibrationRole: CalibrationRole,
        oid:             Observation.Id
      ): NonEmptyString =
        NonEmptyString.unsafeFrom(s"${config.modeTypeTag}/${calibrationRole.tag}/${oid.show}")

      private def findSystemGroupForObservation(
        tree: GroupTree,
        oid:  Observation.Id
      ): Option[Group.Id] =
        tree.findGroupContaining(
          oid,
          b => b.system && b.calibrationRoles.exists(_ == CalibrationRole.Telluric)
        )

      private def findParentGroupForObservation(
        tree: GroupTree,
        oid:  Observation.Id
      ): Option[Group.Id] =
        tree.findGroupContaining(oid, b => !b.system)

      private def f2TelluricGroup(
        pid:           Program.Id,
        config:        CalibrationConfigSubset,
        oid:           Observation.Id,
        parentGroupId: Option[Group.Id]
      )(using Transaction[F]): F[Result[Group.Id]] =
        GroupService.instantiate(emailConfig, httpClient).createGroup(
          CreateGroupInput(
            programId = pid.some,
            proposalReference = none,
            programReference = none,
            SET = GroupPropertiesInput.Create(
              name = groupNameForObservation(config, CalibrationRole.Telluric, oid).some,
              description = none,
              minimumRequired = none,
              ordered = false,
              minimumInterval = none,
              maximumInterval = none,
              parentGroupId = parentGroupId,
              parentGroupIndex = none,
              existence = Existence.Present
            ),
            initialContents = List(Right(oid))
          ),
          system = true,
          calibrationRoles = List(CalibrationRole.Telluric)
        )

      private def telluricGroups(tree: GroupTree): List[(Group.Id, List[Observation.Id])] =
        tree.findGroupsWithObservations(b => b.system && b.calibrationRoles.contains(CalibrationRole.Telluric))

      private def findTelluricObservation(gid: Group.Id): F[Option[Observation.Id]] =
        S.session
          .prepareR(Statements.selectTelluricObservation)
          .use(_.option((gid, CalibrationRole.Telluric)))

      private def createTelluricObservation(
        pid:             Program.Id,
        scienceOid:      Observation.Id,
        telluricGroupId: Group.Id
      )(using Transaction[F], SuperUserAccess): F[Result[Observation.Id]] =
        def obsGroupIndex(scienceOid: Observation.Id): F[NonNegShort] =
          S.session
            .prepareR(Statements.selectScienceObservationIndex)
            .use(_.unique(scienceOid))

        def insertTelluricObservation(
          pid:             Program.Id,
          targetId:        Target.Id,
          telluricGroupId: Group.Id,
          telluricIndex:   NonNegShort
        )(using Transaction[F], SuperUserAccess): F[Result[Observation.Id]] =
          // Minimal input to create the telluric obs
          // TODO create a proper telluric target
          val targetEnvironment = TargetEnvironmentInput.Create(
            explicitBase = none,
            asterism = List(targetId).some,
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
          S.observationService
            .createObservation(
              AccessControl.unchecked(obsInput, pid, program_id),
              calibrationRole = CalibrationRole.Telluric.some
            )

        def telluricTargetPlaceholder(
          pid: Program.Id
        )(using Transaction[F], SuperUserAccess): F[Result[Target.Id]] =
          val targetInput = TargetPropertiesInput.Create(
            name = NonEmptyString.unsafeFrom("Telluric Target (TBD)"),
            subtypeInfo = SiderealInput.Create(
              ra = RightAscension.Zero,
              dec = Declination.Zero,
              epoch = Epoch.J2000,
              properMotion = None,
              radialVelocity = None,
              parallax = None,
              catalogInfo = None
            ),
            sourceProfile = SourceProfile.Point(
              SpectralDefinition.BandNormalized(None, SortedMap.empty)
            ),
            existence = Existence.Present
          )

          S.targetService.createTarget(
            AccessControl.unchecked(targetInput, pid, program_id),
            disposition = TargetDisposition.Calibration,
            role = CalibrationRole.Telluric.some
          )

        (for
          scienceIndex  <- ResultT.liftF(obsGroupIndex(scienceOid))
          telluricIndex = NonNegShort.unsafeFrom((scienceIndex.value + 1).toShort)
          targetId      <- ResultT(telluricTargetPlaceholder(pid))
          telluricId    <- ResultT(insertTelluricObservation(pid, targetId, telluricGroupId, telluricIndex))
          _             <- ResultT(syncConfiguration(scienceOid, telluricId))
        yield telluricId).value

      private def syncConfiguration(
        sourceOid: Observation.Id,
        targetOid: Observation.Id
      )(using Transaction[F], SuperUserAccess): F[Result[Unit]] =

        def readObservingModes: F[List[(Observation.Id, Option[ObservingModeType])]] =
          val af = Statements.selectObservingModeTypes(NonEmptyList.of(sourceOid, targetOid))
          S.session
            .prepareR(af.fragment.query(observation_id *: observing_mode_type.opt))
            .use(_.stream(af.argument, 2).compile.toList)

        def extractModes(
          modes: List[(Observation.Id, Option[ObservingModeType])]
        ): Result[(Option[ObservingModeType], Option[ObservingModeType])] =
          modes match
            case (sid, sourceMode) :: (tid, targetMode) :: Nil if (sid === sourceOid && tid === targetOid) =>
              Result((sourceMode, targetMode))
            case _ =>
              Result.failure("Cannot read obs modes for source and target")

        def syncObservationProperties: F[Unit] =
          S.session.executeCommand(Statements.syncObservationConfiguration(sourceOid, targetOid)).void

        def deleteOldTargetMode(tm: Option[ObservingModeType]): F[Unit] =
          tm.traverse(mode => S.observingModeServices.delete(mode, List(targetOid))).void

        def deleteAllExposureTimeModes(sm: Option[ObservingModeType]): F[Unit] =
          sm.traverse(_ => S.exposureTimeModeService.deleteMany(
            NonEmptyList.one(targetOid),
            ExposureTimeModeRole.Requirement,
            ExposureTimeModeRole.Acquisition,
            ExposureTimeModeRole.Science
          )).void

        def updateTargetModeType(sm: Option[ObservingModeType]): F[Unit] =
          sm.traverse(mode =>
            S.session.execute(Statements.updateObservingModeType)(mode.some, mode.instrument.some, targetOid)
          ).void

        def cloneSourceMode(sm: Option[ObservingModeType]): F[Unit] =
          sm.traverse(mode => S.observingModeServices.clone(mode, sourceOid, targetOid)).void

        (for
          modes    <- ResultT.liftF(readObservingModes)                   // Read modes for both observations
          (sm, tm) <- ResultT(extractModes(modes).pure[F])                // Extract source and target modes
          _        <- ResultT.liftF(syncObservationProperties)            // Copy constraint set and science requirements
          _        <- ResultT.liftF(deleteOldTargetMode(tm))              // Delete old mode-specific tables (if target has mode)
          _        <- ResultT.liftF(deleteAllExposureTimeModes(sm))       // Delete all ETMs to avoid duplicates on clone
          _        <- ResultT.liftF(updateTargetModeType(sm))             // Set target's observing mode type and instrument
          _        <- ResultT.liftF(cloneSourceMode(sm))                  // Clone mode-specific tables and all ETMs from source
        yield ()).value

      override def generateCalibrations(
        pid: Program.Id,
        scienceObs: List[ObsExtract[CalibrationConfigSubset]]
      )(using Transaction[F], SuperUserAccess): F[(List[Observation.Id], List[Observation.Id])] =
        val groupService = S.groupService(emailConfig, httpClient)
        val observationService = S.observationService

        def removeOrphanedScienceObservations(
          gid: Group.Id,
          currentObsIds: Set[Observation.Id]
        ): F[Unit] =
          for
            allObsIds   <- groupService.selectAllObservationsInGroup(gid)
            telluricId  <- findTelluricObservation(gid)
            obsToRemove = allObsIds.filterNot(oid => currentObsIds.contains(oid) || telluricId.contains(oid))
            _           <- obsToRemove.traverse_ : oid =>
                            observationService.updateObservations(
                              AccessControl.unchecked(
                                ObservationPropertiesInput.Edit.Empty.copy(group = Nullable.Null),
                                List(oid),
                                observation_id
                              )
                            )
          yield ()

        def deleteEmptyGroups(
          gid: Group.Id,
          groupObsIds: List[Observation.Id],
          currentObsIds: Set[Observation.Id]
        ): F[List[Observation.Id]] =
          groupObsIds
            .filter(currentObsIds.contains)
            .headOption
            .fold {
              for
                telluricId  <- findTelluricObservation(gid)
                _           <- telluricId.traverse_ : oid =>
                                 observationService
                                   .deleteCalibrationObservations(NonEmptyList.one(oid))
                _           <- groupService.deleteSystemGroup(gid).void
              yield telluricId.toList
            }(_ => List.empty.pure[F])

        def readTelluricGroup(
          tree: GroupTree,
          obs: ObsExtract[CalibrationConfigSubset]
        ): F[Result[Group.Id]] =
          // Find the group, if not found create one
          findSystemGroupForObservation(tree, obs.id)
            .fold(
              f2TelluricGroup(pid, obs.data, obs.id, findParentGroupForObservation(tree, obs.id))
            )(gid => Result(gid).pure[F])

        def syncTelluricObservation(
          obs: ObsExtract[CalibrationConfigSubset],
          gid: Group.Id
        ): F[Result[Option[Observation.Id]]] =
          findTelluricObservation(gid).flatMap:
            case Some(telluricId) =>
              syncConfiguration(obs.id, telluricId).as(Result(none[Observation.Id]))
            case _                =>
              createTelluricObservation(pid, obs.id, gid).flatMap:
                case Result.Warning(problems, oid) =>
                  (info"Created telluric observation with warnings: $problems").as(Result(oid.some))
                case Result.Success(oid)           =>
                  Result(oid.some).pure[F]
                case other                         =>
                  other.map(_.some).pure[F]

        def generateTelluricForScience(
          tree: GroupTree,
          obs: ObsExtract[CalibrationConfigSubset]
        ): F[Result[Option[Observation.Id]]] =
          (for
            gid <- ResultT(readTelluricGroup(tree, obs))
            tid <- ResultT(syncTelluricObservation(obs, gid))
          yield tid).value

        (for
          // Read program's group tree
          _               <- ResultT.liftF(info"Recalculating per science calibrations for $pid, instant")
          tree            <- ResultT.liftF(groupService.selectGroups(pid))
          // Find existing telluric groups
          existingGroups  = telluricGroups(tree)
          // Track current science observations
          currentObsIds   = scienceObs.map(_.id).toSet
          _               <- ResultT.liftF(debug"Program $pid has ${currentObsIds.size} science configurations")
          // Remove stale science obs from groups
          _               <- ResultT.liftF:
                               existingGroups.traverse_ : (gid, _) =>
                                 removeOrphanedScienceObservations(gid, currentObsIds)
          // Delete groups with no current science obs
          deleted         <- ResultT.liftF:
                               existingGroups.flatTraverse: (gid, obsIds) =>
                                 deleteEmptyGroups(gid, obsIds, currentObsIds)
          // Create/sync telluric for each science obs
          added           <- scienceObs.traverse(obs => ResultT(generateTelluricForScience(tree, obs)))
          _               <- ResultT.liftF:
                               debug"Program $pid add ${added} and removed $deleted calibrations"
        yield (added.flatten, deleted)).value.orError

      private object Statements:

        val selectTelluricObservation: Query[(Group.Id, CalibrationRole), Observation.Id] =
          sql"""
            SELECT c_observation_id
            FROM   t_observation
            WHERE  c_group_id         = $group_id
              AND  c_calibration_role = $calibration_role
            LIMIT 1
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
              c_cloud_extinction = source.c_cloud_extinction,
              c_image_quality = source.c_image_quality,
              c_sky_background = source.c_sky_background,
              c_water_vapor = source.c_water_vapor,
              c_air_mass_min = source.c_air_mass_min,
              c_air_mass_max = source.c_air_mass_max,
              c_hour_angle_min = source.c_hour_angle_min,
              c_hour_angle_max = source.c_hour_angle_max,
              c_spec_wavelength = source.c_spec_wavelength,
              c_spec_resolution = source.c_spec_resolution,
              c_spec_wavelength_coverage = source.c_spec_wavelength_coverage,
              c_spec_focal_plane = source.c_spec_focal_plane,
              c_spec_focal_plane_angle = source.c_spec_focal_plane_angle,
              c_spec_capability = source.c_spec_capability,
              c_img_minimum_fov = source.c_img_minimum_fov,
              c_img_narrow_filters = source.c_img_narrow_filters,
              c_img_broad_filters = source.c_img_broad_filters,
              c_img_combined_filters = source.c_img_combined_filters
            FROM t_observation source
            WHERE source.c_observation_id = $observation_id
              AND target.c_observation_id = $observation_id
          """.apply(sourceOid, targetOid)
