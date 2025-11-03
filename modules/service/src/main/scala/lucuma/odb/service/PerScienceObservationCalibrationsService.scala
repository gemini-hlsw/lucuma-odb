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
        parentGroupId: Option[Group.Id],
        parentIndex:   Option[NonNegShort]
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
              parentGroupIndex = parentIndex,
              existence = Existence.Present
            ),
            initialContents = List(Right(oid))
          ),
          system = true,
          calibrationRoles = List(CalibrationRole.Telluric)
        )

      private def telluricGroups(tree: GroupTree): List[(Group.Id, List[(Observation.Id, NonNegShort)])] =
        tree.collectObservations(b => b.system && b.calibrationRoles.contains(CalibrationRole.Telluric))

      private def observationsToMove(
        allObsInGroups: Map[Group.Id, List[(Observation.Id, NonNegShort)]],
        toUnlink:       Set[Observation.Id],
        groupLocations: Map[Group.Id, (Option[Group.Id], NonNegShort)]
      ): List[(Observation.Id, Option[Group.Id], Option[NonNegShort])] =
        allObsInGroups.toList.flatMap: (gid, obsWithIndices) =>
          val toMove = obsWithIndices.filter((o, _) => toUnlink.exists(o === _))
          val (parentGroupId, parentIndex) =
            groupLocations.get(gid)
              .map { case (pid, idx) => (pid, idx.some) }
              .getOrElse((none, none))
          toMove.map((oid, _) => (oid, parentGroupId, parentIndex))

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
          // Obervations to remove from telluric groups
          toUnlink          = allObsInGroups.values.flatten.map(_._1).filterNot(a => currentObsIds.exists(_ === a)).toSet
          // Query group locations
          groupLocations    <- Statements.queryGroupLocations(allObsInGroups.keys.toList)
          // Collect all observations to move with their target locations
          toMove            = observationsToMove(allObsInGroups, toUnlink, groupLocations)
          // Move all observations in a single database call
          _                 <- Statements.moveObservations(toMove)
          // Compute which groups are now empty
          emptyGroupIds     = allObsInGroups.collect:
                                case (gid, obsWithIndices) if obsWithIndices.forall((o, _) => toUnlink.exists(_ === o)) => gid
          // Delete empty telluric groups using deleteSystemGroup
          _                 <- emptyGroupIds.toList.traverse_ : gid =>
                                 groupService.deleteSystemGroup(pid, gid)
          // Reload tree once for group creation step
          currentGroups  <- groupService.selectGroups(pid)
          // Build observation index map from all parent groups (not just system groups)
          allObsWithIndices = currentGroups.collectObservations(_ => true)
          obsIndexMap       = allObsWithIndices.flatMap((_, obs) => obs).toMap
          // Create or verify system groups for current F2 observations
          _                 <- scienceObs.traverse_ : obs =>
                                 findSystemGroupForObservation(currentGroups, obs.id)
                                   .fold(
                                     newTelluricGroup(
                                       pid,
                                       obs.data,
                                       obs.id,
                                       findParentGroupForObservation(currentGroups, obs.id),
                                       obsIndexMap.get(obs.id)
                                     ).void
                                   )(_ => ().pure[F])
        yield List.empty // no calibs yet

      object Statements:

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
