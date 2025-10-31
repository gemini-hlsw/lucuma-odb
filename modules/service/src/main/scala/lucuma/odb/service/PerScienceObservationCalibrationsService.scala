// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import lucuma.core.enums.CalibrationRole
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.util.TimeSpan
import lucuma.odb.Config
import lucuma.odb.data.Existence
import lucuma.odb.data.GroupTree
import lucuma.odb.graphql.input.CreateGroupInput
import lucuma.odb.graphql.input.GroupPropertiesInput
import lucuma.odb.service.Services.ServiceAccess
import lucuma.odb.util.Codecs.*
import org.http4s.client.Client
import skunk.AppliedFragment
import skunk.Query
import skunk.Transaction
import skunk.syntax.all.*

trait PerScienceObservationCalibrationsService[F[_]]:

  def generateCalibrations(
    pid: Program.Id,
    scienceObs: List[ObsExtract[CalibrationConfigSubset]]
  )(using Transaction[F], ServiceAccess): F[List[Observation.Id]]

object PerScienceObservationCalibrationsService:
  def instantiate[F[_]: {Concurrent, Services as S}](emailConfig: Config.Email, httpClient: Client[F]): PerScienceObservationCalibrationsService[F] =
    new PerScienceObservationCalibrationsService[F] with CalibrationObservations:

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
        parentGroupId: Option[Group.Id]
      )(using Transaction[F]): F[Result[Group.Id]] =
        S.groupService(emailConfig, httpClient).createGroup(
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
              parentGroupIndex = none,
              existence = Existence.Present
            ),
            initialContents = List(Right(oid))
          ),
          system = true,
          calibrationRoles = List(CalibrationRole.Telluric)
        )

      private def telluricGroups(tree: GroupTree): List[(Group.Id, List[Observation.Id])] =
        tree.collectObservations(b => b.system && b.calibrationRoles.contains(CalibrationRole.Telluric))

      override def generateCalibrations(
        pid:        Program.Id,
        scienceObs: List[ObsExtract[CalibrationConfigSubset]]
      )(using Transaction[F], ServiceAccess): F[List[Observation.Id]] =
        val groupService  = S.groupService(emailConfig, httpClient)
        val currentObsIds = scienceObs.map(_.id).toSet

        for
          _                 <- S.session.execute(sql"set constraints all deferred".command)
          // Telluric groups with all observations
          telluricGroupList <- groupService.selectGroups(pid, obsFilter = void"true").map(telluricGroups)
          allObsInGroups    = telluricGroupList.toMap
          // Obervations to remove (not telluric anymore)
          toUngroup         = allObsInGroups.values.flatten.filterNot(a => currentObsIds.exists(_ === a))
          // Query group locations (batch query)
          groupLocations    <- allObsInGroups.keys.toList match
                                 case Nil => Map.empty[Group.Id, (Option[Group.Id], NonNegShort)].pure[F]
                                 case gids =>
                                   S.session.prepareR(Statements.groupLocations(gids))
                                     .use(_.stream(gids, 1024).compile.toList)
                                     .map(_.map { case (gid, parentId, parentIndex) =>
                                       gid -> ((parentId, parentIndex))
                                     }.toMap)
          obsToUnlinkSet    = toUngroup.toSet
          // Move observations to their group's parent location (batch by group)
          _                 <- allObsInGroups.toList.traverse_ { case (gid, obsIds) =>
                                 val toMove = obsIds.filter(obsToUnlinkSet.contains)
                                 NonEmptyList.fromList(toMove).traverse_ { nel =>
                                   val (parentGroupId, parentIndex) =
                                     groupLocations.get(gid)
                                       .map { case (pid, idx) => (pid, idx.some) }
                                       .getOrElse((none, none))
                                   val af = Statements.moveObservationsBatch(nel.toList, parentGroupId, parentIndex)
                                   S.session.prepareR(af.fragment.query(void))
                                     .use(_.stream(af.argument, 1024).compile.drain)
                                 }
                               }
          // Compute which groups are now empty
          emptyGroupIds     = allObsInGroups.collect:
                                case (gid, obsIds) if obsIds.forall(o => obsToUnlinkSet.exists(_ === o)) => gid
          // Delete empty system groups using deleteSystemGroup
          _                 <- emptyGroupIds.toList.traverse_ : gid =>
                                 groupService.deleteSystemGroup(pid, gid)
          // Reload tree once for group creation step
          currentGroups  <- groupService.selectGroups(pid)
          // Create or verify system groups for current F2 observations
          _                 <- scienceObs.traverse_ : obs =>
                                 findSystemGroupForObservation(currentGroups, obs.id)
                                   .fold(
                                     newTelluricGroup(
                                       pid,
                                       obs.data,
                                       obs.id,
                                       findParentGroupForObservation(currentGroups, obs.id)
                                     ).void
                                   )(_ => ().pure[F])
        yield List.empty // no calibs yet

      object Statements:
        // Filter to fetch only telluric system groups
        val telluricGroup: AppliedFragment =
          void"c_system = true AND c_calibration_roles = ARRAY['telluric']::e_calibration_role[]"

        def groupLocations(gids: List[Group.Id]): Query[gids.type, (Group.Id, Option[Group.Id], NonNegShort)] =
          sql"""
            select c_group_id, c_parent_id, c_parent_index
            from t_group
            where c_group_id in (${group_id.list(gids)})
          """.query(group_id *: group_id.opt *: int2_nonneg).contramap(_ => gids)

        def moveObservationsBatch(
          oids: List[Observation.Id],
          parentGroupId: Option[Group.Id],
          parentIndex: Option[NonNegShort]
        ): AppliedFragment =
          sql"""
            select group_move_observation(c_observation_id, ${group_id.opt}, ${int2_nonneg.opt})
            from t_observation
            where c_observation_id in (${observation_id.list(oids)})
          """.apply(parentGroupId, parentIndex, oids)
