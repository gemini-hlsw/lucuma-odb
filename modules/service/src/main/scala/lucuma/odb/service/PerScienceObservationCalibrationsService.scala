// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Concurrent
import cats.syntax.all.*
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
        tree.findGroupsWithObservations(b => b.system && b.calibrationRoles.contains(CalibrationRole.Telluric))

      override def generateCalibrations(
        pid:        Program.Id,
        scienceObs: List[ObsExtract[CalibrationConfigSubset]]
      )(using Transaction[F], ServiceAccess): F[List[Observation.Id]] =
        val groupService  = S.groupService(emailConfig, httpClient)
        val currentObsIds = scienceObs.map(_.id).toSet

        for
          _                 <- S.session.execute(sql"set constraints all deferred".command)
          // All telluric program groups
          initialTelluricGroups <- groupService.selectGroups(pid).map(telluricGroups)
          // Query database directly for ALL observations in ALL telluric groups in a single query
          // This is necessary because the tree only includes observations with c_existence = 'present'
          allObsInGroups    <- initialTelluricGroups.map(_._1) match
                                 case Nil => Map.empty[Group.Id, List[Observation.Id]].pure[F]
                                 case gids =>
                                   val enc = group_id.list(gids.length)
                                   S.session.prepareR(
                                     sql"""
                                       SELECT c_group_id, c_observation_id
                                       FROM t_observation
                                       WHERE c_group_id IN ($enc)
                                     """.query(group_id *: observation_id)
                                   ).use { ps =>
                                     ps.stream(gids, 1024)
                                       .compile
                                       .toList
                                       .map(_.groupMap(_._1)(t => t._2))
                                   }
          // Collect all observations that need unlinking (not in current science set)
          obsToUnlink       = allObsInGroups.values.flatten.filterNot(currentObsIds.contains).toList
          // Batch unlink all observations using group_move_observation
          _                 <- obsToUnlink.traverse_ { oid =>
                                 S.session.prepareR(sql"SELECT group_move_observation($observation_id, ${group_id.opt}, ${int2_nonneg.opt})".query(void)).use { ps =>
                                   ps.unique((oid, None, None))
                                 }
                               }
          // Reload tree to see which groups are now empty after unlinking
          treeAfterUnlink   <- groupService.selectGroups(pid)
          existingAfter     = telluricGroups(treeAfterUnlink)
          // Delete empty system groups
          // Note: We must use deleteSystemGroup to properly manage parent group indices
          _                 <- existingAfter
                                 .filter(_._2.isEmpty)
                                 .traverse_ { (gid, _) =>
                                   groupService.deleteSystemGroup(pid, gid).void
                                 }
          // Create or verify system groups for current F2 observations
          _                 <- scienceObs.traverse_ { obs =>
                                 findSystemGroupForObservation(treeAfterUnlink, obs.id)
                                   .fold(
                                     f2TelluricGroup(pid, obs.data, obs.id, findParentGroupForObservation(treeAfterUnlink, obs.id)).void
                                   )(_ => ().pure[F])
                               }
        yield List.empty // no calibs yet

      object Statements:
        val m = 1
        // val GroupObs: Query[(Program.Id, Create), ProgramNote.Id] = ???
                                   // val enc = group_id.list(gids.length)
                                   // S.session.prepareR(
                                   //   sql"""
                                   //     SELECT c_group_id, c_observation_id
                                   //     FROM t_observation
                                   //     WHERE c_group_id IN ($enc)
                                   //   """.query(group_id *: observation_id)
                                   // ).use { ps =>
                                   //   ps.stream(gids, 1024)
                                   //     .compile
                                   //     .toList
                                   //     .map(_.groupMap(_._1)(t => t._2))
                                   // }
