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
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.input.CreateGroupInput
import lucuma.odb.graphql.input.GroupPropertiesInput
import lucuma.odb.graphql.input.ObservationPropertiesInput
import lucuma.odb.graphql.mapping.AccessControl
import lucuma.odb.service.Services.ServiceAccess
import lucuma.odb.util.Codecs.*
import natchez.Trace
import org.http4s.client.Client
import skunk.Transaction

trait PerScienceObservationCalibrationsService[F[_]]:

  def generateCalibrations(
    pid: Program.Id,
    scienceObs: List[ObsExtract[CalibrationConfigSubset]]
  )(using Transaction[F], ServiceAccess): F[List[Observation.Id]]

object PerScienceObservationCalibrationsService:
  def instantiate[F[_]: {Concurrent, Services as S, Trace}](emailConfig: Config.Email, httpClient: Client[F]): PerScienceObservationCalibrationsService[F] =
    new PerScienceObservationCalibrationsService[F] with CalibrationObservations:

      private def groupNameForObservation(
        config: CalibrationConfigSubset,
        calibrationRole: CalibrationRole,
        oid: Observation.Id
      ): NonEmptyString =
        NonEmptyString.unsafeFrom(s"${config.modeType.dbTag}/${calibrationRole.tag}/${oid.show}")

      private def findSystemGroupForObservation(
        tree: GroupTree,
        oid: Observation.Id
      ): Option[Group.Id] =
        tree.findGroupContaining(
          oid,
          b => b.system && b.calibrationRoles.exists(_ == CalibrationRole.Telluric)
        )

      private def findParentGroupForObservation(
        tree: GroupTree,
        oid: Observation.Id
      ): Option[Group.Id] =
        tree.findGroupContaining(oid, b => !b.system)

      private def f2TelluricGroup(
        pid: Program.Id,
        config: CalibrationConfigSubset,
        oid: Observation.Id,
        parentGroupId: Option[Group.Id]
      )(using Transaction[F]): F[Group.Id] =
        GroupService.instantiate(emailConfig, httpClient).createGroup(
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
        ).flatMap:
          case Result.Success(gid) => gid.pure[F]
          case _ =>
            new RuntimeException(s"Failed to create F2 group").raiseError[F, Group.Id]

      private def telluricGroups(tree: GroupTree): List[(Group.Id, List[Observation.Id])] =
        tree.findGroupsWithObservations(b => b.system && b.calibrationRoles.contains(CalibrationRole.Telluric))

      override def generateCalibrations(
        pid: Program.Id,
        scienceObs: List[ObsExtract[CalibrationConfigSubset]]
      )(using Transaction[F], ServiceAccess): F[List[Observation.Id]] =
        val groupService = S.groupService(emailConfig, httpClient)
        val observationService = ObservationService.instantiate[F]

        for
          tree              <- groupService.selectGroups(pid)
          existingGroups    = telluricGroups(tree)
          currentObsIds     = scienceObs.map(_.id).toSet
          // find observations that need cleanup
          _                 <- existingGroups.traverse_ : (gid, _) =>
                                 // Get all observations in this group from the database
                                 groupService.selectAllObservationsInGroup(gid).flatMap: allObsIds =>
                                   allObsIds
                                     .filterNot(currentObsIds.contains)
                                     .traverse_ : oid =>
                                       observationService.updateObservations(
                                         Services.asSuperUser(
                                           AccessControl.unchecked(
                                             ObservationPropertiesInput.Edit.Empty.copy(group = Nullable.Null),
                                             List(oid),
                                             observation_id
                                           )
                                         )
                                       )
          // Clean up empty system groups
          _                 <- existingGroups.traverse_ : (gid, groupObsIds) =>
                                 groupObsIds
                                   .filter(currentObsIds.contains)
                                   .headOption
                                   .fold(groupService.deleteSystemGroup(pid, gid).void)(_ => ().pure[F])
          // Create or verify system groups for current F2 observations
          _                 <- scienceObs.traverse_ { obs =>
                                 findSystemGroupForObservation(tree, obs.id)
                                   .fold(
                                     f2TelluricGroup(pid, obs.data, obs.id, findParentGroupForObservation(tree, obs.id)).void
                                   )(_ => ().pure[F])
                               }
        yield List.empty // no calibs yet
