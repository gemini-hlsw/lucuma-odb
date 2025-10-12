// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Order.catsKernelOrderingForOrder
import cats.data.Nested
import cats.data.NonEmptyList
import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import grackle.Result
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.Site
import lucuma.core.math.Coordinates
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.odb.Config
import lucuma.odb.data.Existence
import lucuma.odb.data.GroupTree
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.input.CreateGroupInput
import lucuma.odb.graphql.input.GroupPropertiesInput
import lucuma.odb.graphql.input.ObservationPropertiesInput
import lucuma.odb.graphql.input.ScienceRequirementsInput
import lucuma.odb.graphql.input.SpectroscopyScienceRequirementsInput
import lucuma.odb.graphql.mapping.AccessControl
import lucuma.odb.sequence.ObservingMode
import lucuma.odb.service.CalibrationConfigSubset.*
import lucuma.odb.service.CalibrationsService.CalObsProps
import lucuma.odb.service.CalibrationsService.CalibrationTypes
import lucuma.odb.service.CalibrationsService.CalibrationsGroupName
import lucuma.odb.service.CalibrationsService.ObsExtract
import lucuma.odb.service.Services.ServiceAccess
import lucuma.odb.service.Services.Syntax.*
import lucuma.odb.util.Codecs.*
import org.http4s.client.Client
import skunk.AppliedFragment
import skunk.Transaction
import skunk.syntax.all.*

import java.time.Instant

trait PerConfigCalibrationsService[F[_]]:
  def generateSharedCalibrations(
    pid: Program.Id,
    allSci: List[ObsExtract[ObservingMode]],
    allCalibs: List[ObsExtract[ObservingMode]],
    calibTargets: List[(Target.Id, String, CalibrationRole, Coordinates)],
    when: Instant
  )(using Transaction[F], ServiceAccess): F[(List[Observation.Id], List[Observation.Id])]

object PerConfigCalibrationsService:
  def instantiate[F[_]: MonadCancelThrow](emailConfig: Config.Email, httpClient: Client[F])(using Services[F]): PerConfigCalibrationsService[F] =
    new PerConfigCalibrationsService[F] with CalibrationObservations:
      private def toConfigForCalibration(all: List[ObsExtract[ObservingMode]]): List[ObsExtract[CalibrationConfigSubset]] =
        all.map(_.map(_.toConfigSubset))

      private def calObsProps(
        calibConfigs: List[ObsExtract[CalibrationConfigSubset]]
      ): Map[CalibrationConfigSubset, CalObsProps] =
        calibConfigs.groupBy(_.data).map { case (k, v) =>
          val w = v.map(_.itc.map(_.spectroscopy.exposureTimeMode.at)).flattenOption match
            case Nil =>
               none[Wavelength]
            case ws  =>
              val pm = ws.map(_.toPicometers.value.value).combineAll / ws.size
              PosInt.from(pm).map(Wavelength(_)).toOption
          k -> CalObsProps(w, v.map(_.band).min)
        }

      private def uniqueConfiguration(
        all: List[ObsExtract[ObservingMode]]
      ): List[CalibrationConfigSubset] = all.map(_.data.toConfigSubset).distinct

      private def calibrationsGroup(pid: Program.Id, size: Int)(using Transaction[F]): F[Option[Group.Id]] =
        if (size > 0) {
          groupService(emailConfig, httpClient).selectGroups(pid).flatMap {
            case GroupTree.Root(_, c) =>
              val existing = c.collectFirst {
                case GroupTree.Branch(gid, _, _, _, Some(CalibrationsGroupName), _, _, _, true) => gid
              }
              existing match {
                case Some(gid) => gid.some.pure[F]
                case None      =>
                  groupService(emailConfig, httpClient).createGroup(
                      input = CreateGroupInput(
                        programId = pid.some,
                        proposalReference = none,
                        programReference = none,
                        SET = GroupPropertiesInput.Create(
                          name = CalibrationsGroupName.some,
                          description = CalibrationsGroupName.some,
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
                  ).map(_.toOption)
              }
            case _ => none.pure[F]
          }
        } else none.pure[F]

      private def setCalibRoleAndGroup(oids: List[Observation.Id], calibrationRole: CalibrationRole): F[Unit] =
        val cmd = void"UPDATE t_observation " |+|
          sql"SET c_calibration_role = $calibration_role "(calibrationRole) |+|
          void"WHERE c_observation_id IN (" |+|
            oids.map(sql"$observation_id").intercalate(void", ") |+| void")"
        session.executeCommand(cmd).void

      private def isCalibrationNeeded(
        scienceConfigs: List[CalibrationConfigSubset],
        calibConfig: CalibrationConfigSubset,
        calibRole: CalibrationRole
      ): Boolean =
        scienceConfigs.exists: sciConfig =>
          CalibrationConfigMatcher.matcherFor(sciConfig, calibRole).configsMatch(sciConfig, calibConfig)

      private def calculateConfigurationsPerRole(
        uniqueSci: List[CalibrationConfigSubset],
        calibs: List[ObsExtract[CalibrationConfigSubset]]
      ): Map[CalibrationRole, List[CalibrationConfigSubset]] =
        CalibrationTypes.map { calibType =>
          val sciConfigs = uniqueSci.map(config =>
            CalibrationConfigMatcher.matcherFor(config, calibType).normalize(config)
          ).distinct
          val calibConfigs = calibs
            .filter(_.role.contains(calibType))
            .map(_.data)
            .map(config => CalibrationConfigMatcher.matcherFor(config, calibType).normalize(config))
            .distinct
          val newConfigs = sciConfigs.diff(calibConfigs)
          (calibType, newConfigs)
        }.toMap

      private def calibObservation(
        calibRole: CalibrationRole,
        site:      Site,
        pid:       Program.Id,
        gid:       Group.Id,
        props:     Map[CalibrationConfigSubset, CalObsProps],
        configs:   List[CalibrationConfigSubset],
        tid:       Target.Id
      )(using Transaction[F]): F[List[Observation.Id]] =
        calibRole match {
          case CalibrationRole.SpectroPhotometric =>
            site match {
              case Site.GN => gmosLongSlitSpecPhotObs(pid, gid, tid, props, configs.collect { case c: GmosNConfigs => c })
              case Site.GS => gmosLongSlitSpecPhotObs(pid, gid, tid, props, configs.collect { case c: GmosSConfigs => c })
            }
          case CalibrationRole.Twilight =>
            site match
              case Site.GN => gmosLongSlitTwilightObs(pid, gid, tid, configs.collect { case c: GmosNConfigs => c })
              case Site.GS => gmosLongSlitTwilightObs(pid, gid, tid, configs.collect { case c: GmosSConfigs => c })
          case _ => List.empty.pure[F]
        }

      private def generateGMOSLSCalibrations(
        pid:           Program.Id,
        props:         Map[CalibrationConfigSubset, CalObsProps],
        configsPerRole: Map[CalibrationRole, List[CalibrationConfigSubset]],
        gnTgt:         CalibrationIdealTargets,
        gsTgt:         CalibrationIdealTargets
      )(using Transaction[F], ServiceAccess): F[List[Observation.Id]] = {
        val allConfigs = configsPerRole.values.flatten.toList
        for {
          cg   <- calibrationsGroup(pid, allConfigs.size)
          oids <- cg.map(g =>
                    configsPerRole.toList.flatTraverse { case (calibType, configs) =>
                      generateCalibrationsForType(pid, g, props, configs, calibType, gnTgt, gsTgt)
                    }
                  ).getOrElse(List.empty.pure[F])
        } yield oids
      }

      private def generateCalibrationsForType(
        pid:       Program.Id,
        gid:       Group.Id,
        props:     Map[CalibrationConfigSubset, CalObsProps],
        configs:   List[CalibrationConfigSubset],
        calibType: CalibrationRole,
        gnTgt:     CalibrationIdealTargets,
        gsTgt:     CalibrationIdealTargets
      )(using Transaction[F], ServiceAccess): F[List[Observation.Id]] = {
        def newCalibs(site: Site, idealTarget: CalibrationIdealTargets): Option[F[List[Observation.Id]]] =
          idealTarget.bestTarget(calibType).map(tgtid =>
            if (configs.nonEmpty) {
              (for {
                cta <- Nested(targetService.cloneTargetInto(tgtid, pid)).map(_._2).value
                o   <- cta.traverse(calibObservation(calibType, site, pid, gid, props, configs, _))
              } yield o).orError
            } else {
              List.empty.pure[F]
            })

        val gnoCalibs = newCalibs(Site.GN, gnTgt)
        val gsoCalibs = newCalibs(Site.GS, gsTgt)

        (gnoCalibs, gsoCalibs).mapN((_, _).mapN(_ ::: _)).getOrElse(List.empty.pure[F]).flatTap { oids =>
          setCalibRoleAndGroup(oids, calibType).whenA(oids.nonEmpty)
        }
      }

      private def removeUnnecessaryCalibrations(
        scienceConfigs: List[CalibrationConfigSubset],
        calibrations:   List[ObsExtract[CalibrationConfigSubset]]
      )(using Transaction[F], ServiceAccess): F[List[Observation.Id]] = {
        val unnecessaryOids = calibrations.collect {
          case ObsExtract(oid, _, _, Some(role), config)
            if !isCalibrationNeeded(scienceConfigs, config, role) => oid
        }

        NonEmptyList.fromList(unnecessaryOids) match {
          case Some(oids) => observationService.deleteCalibrationObservations(oids).as(oids.toList)
          case None       => List.empty.pure[F]
        }
      }

      private def prepareCalibrationUpdates(
        calibrations: List[ObsExtract[CalibrationConfigSubset]],
        removedOids:  List[Observation.Id],
        props:        Map[CalibrationConfigSubset, CalObsProps]
      ): List[(Observation.Id, CalObsProps)] =
        calibrations
          .filterNot { o => removedOids.contains(o.id) }
          .map { o => (o.id, props.get(o.data)) }
          .collect { case (oid, Some(props)) if props.band.isDefined || props.wavelengthAt.isDefined => (oid, props) }

      private def updatePropsAt(
        calibrationUpdates: List[(Observation.Id, CalObsProps)]
      )(using Transaction[F]): F[Unit] =
        calibrationUpdates
          .traverse { (oid, props) =>
            val bandFragment = props.band.map(sql"c_science_band IS DISTINCT FROM $science_band")
            val waveFragment = props.wavelengthAt.map(sql"c_etm_signal_to_noise_at <> $wavelength_pm")
            val needsUpdate  = List(bandFragment, waveFragment).flatten.intercalate(void" OR ")

            services.observationService.updateObservations(
              Services.asSuperUser:
                AccessControl.unchecked(
                  ObservationPropertiesInput.Edit.Empty.copy(
                    scienceBand         = Nullable.orAbsent(props.band),
                    scienceRequirements = props.wavelengthAt.map: w =>
                      ScienceRequirementsInput(
                        exposureTimeMode = Nullable.NonNull(
                          ExposureTimeMode.SignalToNoiseMode(
                            lucuma.core.math.SignalToNoise.unsafeFromBigDecimalExact(100.0),
                            w
                          )
                        ),
                        spectroscopy = SpectroscopyScienceRequirementsInput.Default.some,
                        imaging      = None
                      )
                  ),
                  sql"""
                    SELECT $observation_id
                      FROM t_observation
                    WHERE c_observation_id = $observation_id
                      AND c_calibration_role IS NOT NULL
                      AND ("""(oid, oid) |+| needsUpdate |+| void")"
                )
            )
          }.void

      override def generateSharedCalibrations(
        pid: Program.Id,
        allSci: List[ObsExtract[ObservingMode]],
        allCalibs: List[ObsExtract[ObservingMode]],
        calibTargets: List[(Target.Id, String, CalibrationRole, Coordinates)],
        when: Instant
      )(using Transaction[F], ServiceAccess): F[(List[Observation.Id], List[Observation.Id])] =
        // Filter for GMOS observations (exclude F2)
        val gmosSci = allSci.filterNot(_.data.toConfigSubset.isInstanceOf[Flamingos2Configs])
        val gmosCalibs = toConfigForCalibration(allCalibs).filterNot(_.data.isInstanceOf[Flamingos2Configs])

        // Get unique GMOS configurations
        val uniqueSci = uniqueConfiguration(gmosSci)

        // Extract props from all science observations
        val props = calObsProps(toConfigForCalibration(allSci))

        // Create ideal targets for each site
        val gnTgt = CalibrationIdealTargets(Site.GN, when, calibTargets)
        val gsTgt = CalibrationIdealTargets(Site.GS, when, calibTargets)

        for
          configsPerRole <- calculateConfigurationsPerRole(uniqueSci, gmosCalibs).pure[F]
          removedOids    <- removeUnnecessaryCalibrations(uniqueSci, gmosCalibs)
          addedOids      <- generateGMOSLSCalibrations(pid, props, configsPerRole, gnTgt, gsTgt)
          calibUpdates    = prepareCalibrationUpdates(gmosCalibs, removedOids, props)
          _              <- updatePropsAt(calibUpdates)
        yield (addedOids, removedOids)
