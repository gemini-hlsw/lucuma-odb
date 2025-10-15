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
import lucuma.odb.graphql.input.CreateGroupInput
import lucuma.odb.graphql.input.GroupPropertiesInput
import lucuma.odb.service.Services.ServiceAccess
import org.http4s.client.Client
import skunk.Transaction

trait PerScienceObservationCalibrationsService[F[_]]:

  def generateCalibrations(
    pid: Program.Id,
    f2ScienceObs: List[ObsExtract[CalibrationConfigSubset]]
  )(using Transaction[F], ServiceAccess): F[List[Observation.Id]]

object PerScienceObservationCalibrationsService:
  def instantiate[F[_]: Concurrent](emailConfig: Config.Email, httpClient: Client[F])(using Services[F]): PerScienceObservationCalibrationsService[F] =
    new PerScienceObservationCalibrationsService[F] with CalibrationObservations:

      private def groupNameForObservation(oid: Observation.Id): NonEmptyString =
        NonEmptyString.unsafeFrom(s"F2 Science and Telluric Standard for ${oid.show}")

      private def findSystemGroupForObservation(
        tree: GroupTree,
        oid: Observation.Id
      ): Option[Group.Id] =
        def searchTree(node: GroupTree): Option[Group.Id] =
          node match
            case GroupTree.Root(_, children) =>
              children.flatMap(searchTree).headOption
            case GroupTree.Branch(gid, _, _, children, _, _, _, _, system, calibrationRoles) =>
              val hasObs = children.exists:
                case GroupTree.Leaf(obsId) => obsId == oid
                case _                     => false
              if system && calibrationRoles.contains(CalibrationRole.Telluric) && hasObs then
                Some(gid)
              else
                children.flatMap(searchTree).headOption
            case GroupTree.Leaf(_) =>
              None
        searchTree(tree)

      private def findParentGroupForObservation(
        tree: GroupTree,
        oid: Observation.Id
      ): Option[Group.Id] =
        def searchTree(node: GroupTree): Option[Group.Id] =
          node match
            case GroupTree.Root(_, children) =>
              children.flatMap(searchTree).headOption
            case GroupTree.Branch(gid, _, _, children, _, _, _, _, system, _) =>
              val hasObsDirectly = children.exists:
                case GroupTree.Leaf(obsId) => obsId == oid
                case _                     => false
              if hasObsDirectly && !system then
                Some(gid)
              else
                children.flatMap(searchTree).headOption
            case GroupTree.Leaf(_) =>
              None
        searchTree(tree)

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

      override def generateCalibrations(
        pid: Program.Id,
        f2ScienceObs: List[ObsExtract[CalibrationConfigSubset]]
      )(using Transaction[F], ServiceAccess): F[List[Observation.Id]] =
        for
          tree <- GroupService.instantiate(emailConfig, httpClient).selectGroups(pid)
          _ <- f2ScienceObs.traverse: obs =>
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
