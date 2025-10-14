// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.StellarLibrarySpectrum
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
import lucuma.core.model.UnnormalizedSED
import lucuma.odb.Config
import lucuma.odb.data.Existence
import lucuma.odb.data.GroupTree
import lucuma.odb.graphql.input.CreateGroupInput
import lucuma.odb.graphql.input.GroupPropertiesInput
import lucuma.odb.graphql.input.SiderealInput
import lucuma.odb.graphql.input.TargetPropertiesInput
import lucuma.odb.graphql.mapping.AccessControl
import lucuma.odb.service.CalibrationConfigSubset.Flamingos2Configs
import lucuma.odb.service.CalibrationsService.ObsExtract
import lucuma.odb.service.Services.ServiceAccess
import lucuma.odb.service.Services.Syntax.*
import lucuma.odb.util.Codecs.*
import org.http4s.client.Client
import skunk.AppliedFragment
import skunk.Transaction
import skunk.syntax.all.*

import scala.collection.immutable.SortedMap

trait PerObsCalibrationsService[F[_]]:
  def generatePerObsCalibrations(
    pid: Program.Id,
    f2ScienceObs: List[ObsExtract[CalibrationConfigSubset]]
  )(using Transaction[F], ServiceAccess): F[List[Observation.Id]]

object PerObsCalibrationsService:
  def instantiate[F[_]: MonadCancelThrow](emailConfig: Config.Email, httpClient: Client[F])(using Services[F]): PerObsCalibrationsService[F] =
    new PerObsCalibrationsService[F] with CalibrationObservations:
      private def telluricGroupName(scienceObsId: Observation.Id): NonEmptyString =
        NonEmptyString.unsafeFrom(s"F2 Science and Telluric Standard per $scienceObsId")

      private def findTelluricGroupForScience(
        pid: Program.Id,
        scienceObsId: Observation.Id
      )(using Transaction[F]): F[Option[Group.Id]] =
        val groupName = telluricGroupName(scienceObsId)
        groupService(emailConfig, httpClient).selectGroups(pid).map {
          case GroupTree.Root(_, children) =>
            children.collectFirst {
              case GroupTree.Branch(gid, _, _, _, Some(name), _, _, _, true) if name == groupName => gid
            }
          case _ => none
        }

      private def getOrCreateTelluricTarget(
        pid: Program.Id
      )(using Transaction[F], ServiceAccess): F[Target.Id] =
        Services.asSuperUser:
          targetService.createTarget(
            AccessControl.unchecked(
              TargetPropertiesInput.Create(
                name = NonEmptyString.unsafeFrom("Telluric Standard"),
                subtypeInfo = SiderealInput.Create(
                  ra = RightAscension.Zero,
                  dec = Declination.Zero,
                  epoch = Epoch.J2000,
                  properMotion = none,
                  radialVelocity = none,
                  parallax = none,
                  catalogInfo = none
                ),
                sourceProfile = SourceProfile.Point(
                  SpectralDefinition.BandNormalized(
                    sed = UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.A0V).some,
                    brightnesses = SortedMap.empty
                  )
                ),
                existence = Existence.Present
              ),
              pid,
              program_id
            ),
            TargetDisposition.Calibration,
            CalibrationRole.Telluric.some
          ).orError

      private def getTelluricObservationsInGroup(
        gid: Group.Id
      ): F[List[Observation.Id]] =
        session.execute(sql"""
          SELECT c_observation_id
          FROM t_observation
          WHERE c_group_id = $group_id
            AND c_calibration_role = $calibration_role
        """.query(observation_id))((gid, CalibrationRole.Telluric))

      private def setCalibRoleAndGroup(oids: List[Observation.Id], calibrationRole: CalibrationRole): F[Unit] =
        val cmd = void"UPDATE t_observation " |+|
          sql"SET c_calibration_role = $calibration_role "(calibrationRole) |+|
          void"WHERE c_observation_id IN (" |+|
            oids.map(sql"$observation_id").intercalate(void", ") |+| void")"
        session.executeCommand(cmd).void

      private def ensureTelluricForScience(
        pid: Program.Id,
        scienceObs: ObsExtract[CalibrationConfigSubset],
        telluricTarget: Target.Id
      )(using Transaction[F], ServiceAccess): F[Option[Observation.Id]] =
        scienceObs.data match
          case f2Config: Flamingos2Configs =>
            findTelluricGroupForScience(pid, scienceObs.id).flatMap { existingGroup =>
              existingGroup match
                case Some(gid) =>
                  getTelluricObservationsInGroup(gid).flatMap { tellurics =>
                    tellurics match
                      case telluricOid :: _ =>
                        session.execute(sql"""
                          SELECT c_observation_id
                          FROM t_observation
                          WHERE c_observation_id = $observation_id
                        """.query(observation_id))(telluricOid).flatMap {
                          case _ :: _ => none.pure[F]
                          case Nil =>
                            flamingos2TelluricObs(pid, gid, telluricTarget, f2Config)
                              .flatTap(oid => setCalibRoleAndGroup(List(oid), CalibrationRole.Telluric))
                              .map(_.some)
                        }
                      case Nil =>
                        flamingos2TelluricObs(pid, gid, telluricTarget, f2Config)
                          .flatTap(oid => setCalibRoleAndGroup(List(oid), CalibrationRole.Telluric))
                          .map(_.some)
                  }
                case None =>
                  val groupName = telluricGroupName(scienceObs.id)
                  groupService(emailConfig, httpClient).createGroup(
                    input = CreateGroupInput(
                      programId = pid.some,
                      proposalReference = none,
                      programReference = none,
                      SET = GroupPropertiesInput.Create(
                        name = groupName.some,
                        description = none,
                        minimumRequired = none,
                        ordered = false,
                        minimumInterval = none,
                        maximumInterval = none,
                        parentGroupId = none,
                        parentGroupIndex = none,
                        existence = Existence.Present
                      ),
                      Nil
                    ),
                    system = true
                  ).flatMap {
                    case Result.Success(gid) =>
                      flamingos2TelluricObs(pid, gid, telluricTarget, f2Config)
                        .flatTap(oid => setCalibRoleAndGroup(List(oid), CalibrationRole.Telluric))
                        .map(_.some)
                    case _ => none.pure[F]
                  }
            }
          case _ => none.pure[F]

      override def generatePerObsCalibrations(
        pid: Program.Id,
        f2ScienceObs: List[ObsExtract[CalibrationConfigSubset]]
      )(using Transaction[F], ServiceAccess): F[List[Observation.Id]] =
        for
          telluricTarget <- getOrCreateTelluricTarget(pid)
          addedOids      <- f2ScienceObs.traverse(scienceObs =>
                              ensureTelluricForScience(pid, scienceObs, telluricTarget)
                            ).map(_.flatten)
        yield addedOids

