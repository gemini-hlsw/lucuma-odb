// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Applicative
import cats.Order.catsKernelOrderingForOrder
import cats.data.NonEmptyList
import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
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
import lucuma.odb.data.Existence
import lucuma.odb.data.ExposureTimeModeRole
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
import lucuma.odb.service.CalibrationsService.PerProgramPerConfigCalibrationTypes
import lucuma.odb.service.Services.ServiceAccess
import lucuma.odb.service.Services.Syntax.*
import lucuma.odb.util.Codecs.*
import lucuma.refined.*
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.syntax.*
import skunk.AppliedFragment
import skunk.Transaction
import skunk.syntax.all.*

import java.time.Instant

trait PerProgramPerConfigCalibrationsService[F[_]]:
  def generateCalibrations(
    pid:          Program.Id,
    allSci:       List[ObsExtract[ObservingMode]],
    allCalibs:    List[ObsExtract[ObservingMode]],
    calibTargets: List[(Target.Id, String, CalibrationRole, Coordinates)],
    when:         Instant
  )(using Transaction[F], ServiceAccess): F[(List[Observation.Id], List[Observation.Id])]

object PerProgramPerConfigCalibrationsService:
  val CalibrationsGroupName: NonEmptyString = "Calibrations".refined

  def instantiate[F[_]: {MonadCancelThrow, Services, LoggerFactory as LF}]: PerProgramPerConfigCalibrationsService[F] =
    new PerProgramPerConfigCalibrationsService[F] with CalibrationObservations with WorkflowStateQueries[F]:
      given Logger[F] = LF.getLoggerFromName("per-program-calibrations")

      private def calObsProps(
        calibConfigs: List[ObsExtract[CalibrationConfigSubset]]
      ): Map[CalibrationConfigSubset, CalObsProps] =
        calibConfigs.groupBy(_.data).map { case (k, v) =>
          val w = v.map(_.itc.flatMap(_.spectroscopy.headOption).map(_.exposureTimeMode.at)).flattenOption match
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
          groupService.selectGroups(pid).flatMap {
            case GroupTree.Root(_, c) =>
              val existing = c.collectFirst {
                case GroupTree.Branch(gid, _, _, _, Some(CalibrationsGroupName), _, _, _, true, _) => gid
              }
              existing match {
                case Some(gid) => gid.some.pure[F]
                case None      =>
                  groupService.createGroup(
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
                    system = true,
                    calibrationRoles = List(CalibrationRole.Twilight, CalibrationRole.SpectroPhotometric)
                  ).map(_.toOption)
              }
            case _ => none.pure[F]
          }
        } else none.pure[F]

      // Set the calibration role of the observations in bulk
      private def setCalibRoleAndGroup(oids: List[Observation.Id], calibrationRole: CalibrationRole): F[Unit] =
        session.executeCommand(CalibrationsService.Statements.setCalibRole(oids, calibrationRole)).void

      /**
       * Check if a calibration is actually needed by any science observation.
       */
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
        PerProgramPerConfigCalibrationTypes.map { calibType =>
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
        config:    CalibrationConfigSubset,
        tid:       Target.Id
      )(using Transaction[F], MonadCancelThrow[F]): Option[F[Observation.Id]] =
        (site, calibRole, config) match
          case (Site.GN, CalibrationRole.SpectroPhotometric, c: GmosNConfigs) =>
            gmosLongSlitSpecPhotObs(pid, gid, tid, props, c).some
          case (Site.GS, CalibrationRole.SpectroPhotometric, c: GmosSConfigs) =>
            gmosLongSlitSpecPhotObs(pid, gid, tid, props, c).some
          case (Site.GN, CalibrationRole.Twilight, c: GmosNConfigs)           =>
            gmosLongSlitTwilightObs(pid, gid, tid, c).some
          case (Site.GS, CalibrationRole.Twilight, c: GmosSConfigs)           =>
            gmosLongSlitTwilightObs(pid, gid, tid, c).some
          case _                                                              =>
            none

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
        def newCalibs(site: Site, idealTarget: CalibrationIdealTargets, siteConfigs: List[CalibrationConfigSubset]): Option[F[List[Observation.Id]]] =
          idealTarget.bestTarget(calibType).map: tgtid =>
            siteConfigs.flatTraverse: config =>
              for {
                (_, tid) <- targetService.cloneTargetInto(tgtid, pid).orError
                oid      <- calibObservation(calibType, site, pid, gid, props, config, tid).sequence
              } yield oid.toList

        val gnoCalibs = newCalibs(Site.GN, gnTgt, configs.collect { case g: GmosNConfigs => g })
        val gsoCalibs = newCalibs(Site.GS, gsTgt, configs.collect { case g: GmosSConfigs => g })

        (gnoCalibs, gsoCalibs).mapN((_, _).mapN(_ ::: _)).getOrElse(List.empty.pure[F]).flatTap { oids =>
          setCalibRoleAndGroup(oids, calibType).whenA(oids.nonEmpty)
        }
      }

      private def removeUnnecessaryCalibrations(
        scienceConfigs: List[CalibrationConfigSubset],
        calibrations:   List[ObsExtract[CalibrationConfigSubset]]
      )(using Transaction[F], ServiceAccess): F[List[Observation.Id]] = {
        val candidates = calibrations.collect {
          case ObsExtract(oid, _, _, Some(role), config)
            if !isCalibrationNeeded(scienceConfigs, config, role) => oid
        }

        excludeOngoingAndCompleted(candidates, identity)
        .flatMap: unnecessaryOids =>
          NonEmptyList.fromList(unnecessaryOids) match {
            case Some(oids) => observationService.deleteCalibrationObservations(oids).as(oids.toList)
            case None       => List.empty.pure[F]
          }
      }

      // Update the signal to noise at wavelength for each calbiration observation depending
      // on the average wavelength of the configuration
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
            val etmJoin: AppliedFragment =
              if props.wavelengthAt.isDefined then
                void"""LEFT JOIN t_exposure_time_mode e USING (c_observation_id)"""
              else
                void""

            val bandFragment = props.band.map(sql"o.c_science_band IS DISTINCT FROM $science_band")
            val waveFragment = props.wavelengthAt.map(w => sql"(e.c_signal_to_noise_at <> $wavelength_pm AND e.c_role = $exposure_time_mode_role)".apply(w, ExposureTimeModeRole.Science))
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
                  void"""
                    SELECT DISTINCT c_observation_id
                      FROM t_observation o
                  """ |+| etmJoin |+| sql"""
                    WHERE o.c_observation_id = $observation_id
                      AND o.c_calibration_role IS NOT NULL
                      AND (
                  """.apply(oid) |+| needsUpdate |+| void")"
                )
            )
          }.void

      private def deleteEmptyCalibrationGroup(pid: Program.Id)(using Transaction[F], ServiceAccess): F[Unit] =
        groupService.selectGroups(pid).flatMap:
          case GroupTree.Root(_, children) =>
            children.collectFirst {
              case GroupTree.Branch(gid, _, _, obs, Some(CalibrationsGroupName), _, _, _, true, _)
                if obs.isEmpty => gid
            }.traverse_(groupService.deleteSystemGroup(pid, _))
          case _                           =>
            Applicative[F].unit

      override def generateCalibrations(
        pid: Program.Id,
        allSci: List[ObsExtract[ObservingMode]],
        allCalibs: List[ObsExtract[ObservingMode]],
        calibTargets: List[(Target.Id, String, CalibrationRole, Coordinates)],
        when: Instant
      )(using Transaction[F], ServiceAccess): F[(List[Observation.Id], List[Observation.Id])] =

        // Filter for GMOS observations (exclude F2)
        val gmosSci = allSci.collect(ObsExtract.perProgramFilter)
        val gmosCalibs = toConfigForCalibration(allCalibs).collect(ObsExtract.perProgramCalibrationFilter)

        for {
          // Filter for only 'defined' or 'ready' observations where obscalc has been calculated
          activeGmosSci   <- onlyDefinedAndReady(gmosSci, _.id)
          // unique GMOS configurations
          uniqueSci       = uniqueConfiguration(activeGmosSci)
          // Extract props from all science observations
          props           = calObsProps(toConfigForCalibration(allSci))
          // Create ideal targets for each site
          gnTgt           = CalibrationIdealTargets(Site.GN, when, calibTargets)
          gsTgt           = CalibrationIdealTargets(Site.GS, when, calibTargets)
          configsPerRole  = calculateConfigurationsPerRole(uniqueSci, gmosCalibs)
          _              <- info"===== Recalculating shared calibrations for program ID: $pid, instant $when ====="
          _              <- info"Program $pid has ${uniqueSci.length} science configurations"
          // Remove calibrations that are not needed, basically when a config is removed
          removedOids    <- removeUnnecessaryCalibrations(uniqueSci, gmosCalibs)
          _              <- (info"Program $pid will remove unnecessary calibrations $removedOids").whenA(removedOids.nonEmpty)
          // Generate new calibrations for each unique configuration
          addedOids      <- generateGMOSLSCalibrations(pid, props, configsPerRole, gnTgt, gsTgt)
          _              <- (info"Program $pid added calibrations $addedOids").whenA(addedOids.nonEmpty)
          calibUpdates    = prepareCalibrationUpdates(gmosCalibs, removedOids, props)
          _              <- updatePropsAt(calibUpdates)
          // Delete the calibration group if empty
          _              <- deleteEmptyCalibrationGroup(pid)
        } yield (addedOids, removedOids)
