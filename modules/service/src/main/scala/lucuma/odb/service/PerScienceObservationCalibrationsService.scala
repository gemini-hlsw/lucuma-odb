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

      private def groupNameForObservation(oid: Observation.Id): NonEmptyString =
        NonEmptyString.unsafeFrom(s"F2 Science and Telluric Standard for ${oid.show}")

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

      private def createF2TelluricGroup(
        pid: Program.Id,
        oid: Observation.Id,
        parentGroupId: Option[Group.Id]
      )(using Transaction[F]): F[Group.Id] =
        GroupService.instantiate(emailConfig, httpClient).createGroup(
          CreateGroupInput(
            programId = pid.some,
            proposalReference = none,
            programReference = none,
            SET = GroupPropertiesInput.Create(
              name = groupNameForObservation(oid).some,
              description = NonEmptyString.unsafeFrom("System group for F2 observation and its telluric calibration").some,
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
          // find observations that need cleanup (including DELETED observations not in the tree)
          _                 <- existingGroups.traverse: (groupId, _) =>
                                 // Get ALL observations in this group from the database (including DELETED ones)
                                 groupService.selectAllObservationsInGroup(groupId).flatMap: allGroupObs =>
                                   val obsoleteObs = allGroupObs.filterNot(currentObsIds.contains)
                                   if obsoleteObs.nonEmpty then
                                     // Remove observations from this system group and move them to parent or null
                                     obsoleteObs.traverse: oid =>
                                       observationService.updateObservations:
                                         Services.asSuperUser:
                                           AccessControl.unchecked(
                                             ObservationPropertiesInput.Edit.Empty.copy(group = Nullable.Null),
                                             List(oid),
                                             observation_id
                                           )
                                     .void
                                   else
                                     ().pure[F]
          // Clean up empty system groups
          _ <- existingGroups.traverse: (groupId, groupObsIds) =>
            val remainingObs = groupObsIds.filter(currentObsIds.contains)
            if remainingObs.isEmpty then
              groupService.deleteSystemGroup(groupId)
            else
              ().pure[F]

          // Create or verify system groups for current F2 observations
          _ <- scienceObs.traverse: obs =>
            val existingSystemGroup = findSystemGroupForObservation(tree, obs.id)
            existingSystemGroup match
              case Some(_) =>
                // Observation already in correct system group
                ().pure[F]
              case None =>
                // Need to create system group
                val parentGroup = findParentGroupForObservation(tree, obs.id)
                createF2TelluricGroup(pid, obs.id, parentGroup).void
        yield List.empty
