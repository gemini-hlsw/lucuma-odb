// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Applicative
import cats.Order.catsKernelOrderingForOrder
import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.Site
import lucuma.core.math.Coordinates
import lucuma.core.math.SignalToNoise
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
import lucuma.odb.graphql.input.GmosIfuInput
import lucuma.odb.graphql.input.GmosLongSlitInput
import lucuma.odb.graphql.input.GroupPropertiesInput
import lucuma.odb.graphql.input.ObservationPropertiesInput
import lucuma.odb.graphql.input.ObservingModeInput
import lucuma.odb.graphql.input.ScienceRequirementsInput
import lucuma.odb.graphql.input.SpectroscopyScienceRequirementsInput
import lucuma.odb.graphql.mapping.AccessControl
import lucuma.odb.sequence.ObservingMode
import lucuma.odb.sequence.data.ItcInput
import lucuma.odb.service.CalibrationConfigSubset.*
import lucuma.odb.service.CalibrationsService.PerProgramPerConfigCalibrationTypes
import lucuma.odb.service.Services.ServiceAccess
import lucuma.odb.service.Services.Syntax.*
import lucuma.odb.util.Codecs.*
import lucuma.refined.*
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.syntax.*
import org.typelevel.otel4s.trace.Tracer
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

  def instantiate[F[_]: {Concurrent, Services, Tracer, LoggerFactory as LF}]: PerProgramPerConfigCalibrationsService[F] =
    new PerProgramPerConfigCalibrationsService[F] with CalibrationObservations with WorkflowStateQueries[F]:
      given Logger[F] = LF.getLoggerFromName("per-program-calibrations")

      private def calObsProps(
        calibConfigs: List[ObsExtract[CalibrationConfigSubset]],
        calibType:    CalibrationRole
      ): Map[CalibrationConfigSubset, CalObsProps] =
        calibConfigs.groupBy(ex => CalibrationConfigMatcher.matcherFor(ex.data, calibType).normalize(ex.data)).map: (k, v) =>
          val w = v.map(ex => ex.itc.flatMap(ItcInput.spectroscopy.getOption).map(_.science.mode.exposureTimeMode.at)).flattenOption match
            case Nil =>
               none[Wavelength]
            case ws  =>
              val pm = ws.map(_.toPicometers.value.value).combineAll / ws.size
              PosInt.from(pm).map(Wavelength(_)).toOption
          // Highest priority band (band 1 beats band 2, …) among the science
          // observations sharing the configuration.  Observations with no band
          // assigned are ignored rather than dragging the result to "no band",
          // which would leave the calibration unschedulable.
          k -> CalObsProps(w, v.flatMap(_.band).minOption)

      private def uniqueConfiguration(
        all: List[ObsExtract[ObservingMode]]
      ): List[CalibrationConfigSubset] = all.map(_.data.toConfigSubset).distinct

      private def calibrationsGroup(pid: Program.Id, size: Int)(using Transaction[F]): F[Option[Group.Id]] =
        if (size > 0) {
          groupService.selectGroups(pid).flatMap {
            case GroupTree.Root(_, c) =>
              val existing = c.collectFirst {
                case GroupTree.Branch(groupId = gid, name = Some(CalibrationsGroupName), system = true) => gid
              }
              existing match {
                case Some(gid) => gid.some.pure[F]
                case None      =>
                  groupService.createGroup(
                      input = Services.asSuperUser:
                        AccessControl.unchecked(
                          GroupService.NewGroup(
                            SET = GroupPropertiesInput.Create(
                              name = CalibrationsGroupName.some,
                              description = CalibrationsGroupName.some,
                              minimumRequired = none,
                              ordered = false,
                              minimumInterval = none,
                              maximumInterval = none,
                              sameNight = false,
                              parentGroupId = none,
                              parentGroupIndex = none,
                              existence = Existence.Present
                            ),
                            initialContents = Nil
                          ),
                          pid,
                          program_id
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
      )(using Transaction[F], Concurrent[F]): Option[F[Observation.Id]] =
        (site, calibRole, config) match
          case (Site.GN, CalibrationRole.SpectroPhotometric, c: (GmosNConfigs | GmosNIfuConfigs)) =>
            gmosSpecPhotObs(pid, gid, tid, props, c).some
          case (Site.GS, CalibrationRole.SpectroPhotometric, c: (GmosSConfigs | GmosSIfuConfigs)) =>
            gmosSpecPhotObs(pid, gid, tid, props, c).some
          case (Site.GN, CalibrationRole.Twilight, c: (GmosNConfigs | GmosNIfuConfigs))           =>
            gmosTwilightObs(pid, gid, tid, props, c).some
          case (Site.GS, CalibrationRole.Twilight, c: (GmosSConfigs | GmosSIfuConfigs))           =>
            gmosTwilightObs(pid, gid, tid, props, c).some
          case _                                                                                    =>
            none

      private def generateGMOSLSCalibrations(
        pid:            Program.Id,
        propsByRole:    Map[CalibrationRole, Map[CalibrationConfigSubset, CalObsProps]],
        configsPerRole: Map[CalibrationRole, List[CalibrationConfigSubset]],
        gnTgt:          CalibrationIdealTargets,
        gsTgt:          CalibrationIdealTargets
      )(using Transaction[F], ServiceAccess): F[List[Observation.Id]] = {
        val allConfigs = configsPerRole.values.flatten.toList
        for {
          cg   <- calibrationsGroup(pid, allConfigs.size)
          oids <- cg.map(g =>
                    configsPerRole.toList.flatTraverse: (calibType, configs) =>
                      generateCalibrationsForType(pid, g, propsByRole.getOrElse(calibType, Map.empty), configs, calibType, gnTgt, gsTgt)
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

        val gnoCalibs = newCalibs(Site.GN, gnTgt, configs.collect { case g: (GmosNConfigs | GmosNIfuConfigs) => g })
        val gsoCalibs = newCalibs(Site.GS, gsTgt, configs.collect { case g: (GmosSConfigs | GmosSIfuConfigs) => g })

        (gnoCalibs, gsoCalibs).mapN((_, _).mapN(_ ::: _)).getOrElse(List.empty.pure[F]).flatTap { oids =>
          setCalibRoleAndGroup(oids, calibType).whenA(oids.nonEmpty)
        }
      }

      private def removeUnnecessaryCalibrations(
        scienceConfigs: List[CalibrationConfigSubset],
        calibrations:   List[ObsExtract[CalibrationConfigSubset]]
      )(using Transaction[F], ServiceAccess): F[List[Observation.Id]] = {
        val candidates = calibrations.collect {
          case ObsExtract(id = oid, role = Some(role), data = config)
            if !isCalibrationNeeded(scienceConfigs, config, role) => oid
        }

        excludeOngoingAndCompleted(candidates, identity)
        .flatMap: unnecessaryOids =>
          NonEmptyList.fromList(unnecessaryOids) match {
            case Some(oids) => observationService.deleteCalibrationObservations(oids).as(oids.toList)
            case None       => List.empty.pure[F]
          }
      }

      // Calibrations whose band or ETM may need to follow the science observations
      private def prepareCalibrationUpdates(
        calibrations: List[ObsExtract[CalibrationConfigSubset]],
        removedOids:  List[Observation.Id],
        propsByRole:  Map[CalibrationRole, Map[CalibrationConfigSubset, CalObsProps]]
      ): F[List[(ObsExtract[CalibrationConfigSubset], CalObsProps)]] =
        val candidates =
          calibrations
            .filterNot { o => removedOids.contains(o.id) }
            .flatMap { o => o.role.flatMap(calibrationRole => propsByRole.get(calibrationRole).flatMap(_.get(o.data))).tupleLeft(o) }
        excludeOngoingAndCompleted(candidates, _._1.id)

      // A partial GMOS spectroscopy mode edit that touches only the science ETM
      private def gmosScienceEtmEdit(modeType: ObservingModeType, etm: ExposureTimeMode): ObservingModeInput.Edit =
        val ls  = GmosLongSlitInput.Edit.Common.AllUndefined.copy(exposureTimeMode = etm.some)
        val ifu = GmosIfuInput.Edit.Common.AllUndefined.copy(exposureTimeMode = etm.some)
        val edit = ObservingModeInput.Edit(
          exchange           = none,
          flamingos2Imaging  = none,
          flamingos2LongSlit = none,
          flamingos2Mos      = none,
          ghostIfu           = none,
          gmosNorthIfu       = none,
          gmosNorthImaging   = none,
          gmosNorthLongSlit  = none,
          gmosNorthMos       = none,
          gmosSouthIfu       = none,
          gmosSouthImaging   = none,
          gmosSouthLongSlit  = none,
          gmosSouthMos       = none,
          gnirsImaging       = none,
          gnirsSpectroscopy  = none,
          igrins2LongSlit    = none,
          visitor            = none
        )
        modeType match
          case ObservingModeType.GmosNorthLongSlit =>
            edit.copy(gmosNorthLongSlit = GmosLongSlitInput.Edit.North(none, Nullable.Absent, none, ls, none).some)
          case ObservingModeType.GmosSouthLongSlit =>
            edit.copy(gmosSouthLongSlit = GmosLongSlitInput.Edit.South(none, Nullable.Absent, none, ls, none).some)
          case ObservingModeType.GmosNorthIfu      =>
            edit.copy(gmosNorthIfu = GmosIfuInput.Edit.North(none, Nullable.Absent, none, none, ifu).some)
          case ObservingModeType.GmosSouthIfu      =>
            edit.copy(gmosSouthIfu = GmosIfuInput.Edit.South(none, Nullable.Absent, none, none, ifu).some)
          case other                               =>
            sys.error(s"gmosScienceEtmEdit: unexpected observing mode type $other")

      private def updatePropsAt(
        calibrationUpdates: List[(Observation.Id, CalObsProps)]
      )(using Transaction[F]): F[Unit] =
        calibrationUpdates.groupBy(_._2).toList
          .traverse_ : (props, entries) =>
            val oids = entries.map(_._1)

            val etmJoin: AppliedFragment =
              if props.wavelengthAt.isDefined then
                void"""LEFT JOIN t_exposure_time_mode e USING (c_observation_id)"""
              else
                void""

            val bandFragment = props.band.map(sql"o.c_science_band IS DISTINCT FROM $science_band")
            val waveFragment = props.wavelengthAt.map(w => sql"(e.c_signal_to_noise_at <> $wavelength_pm AND e.c_role = $exposure_time_mode_role)".apply(w, ExposureTimeModeRole.Science))
            val needsUpdate  = List(bandFragment, waveFragment).flatten.intercalate(void" OR ")

            val oidInClause =
              void"o.c_observation_id IN (" |+| oids.map(sql"$observation_id").intercalate(void", ") |+| void")"

            def selection(extraFilter: AppliedFragment): AppliedFragment =
              void"""
                SELECT DISTINCT c_observation_id
                  FROM t_observation o
              """               |+| etmJoin     |+|
              void""" WHERE """ |+| oidInClause |+|
              void""" AND o.c_calibration_role IS NOT NULL AND (""" |+| needsUpdate |+| void")" |+| extraFilter

            // Sync the requirements ETM
            val requirementUpdate =
              services.observationService.updateObservations(
                Services.asSuperUser:
                  AccessControl.unchecked(
                    ObservationPropertiesInput.Edit.Empty.copy(
                      scienceBand         = Nullable.orAbsent(props.band),
                      scienceRequirements = props.wavelengthAt.map: w =>
                        ScienceRequirementsInput(
                          exposureTimeMode = Nullable.NonNull(
                            ExposureTimeMode.SignalToNoiseMode(SignalToNoise.unsafeFromBigDecimalExact(100.0), w)
                          ),
                          spectroscopy = SpectroscopyScienceRequirementsInput.Default.some,
                          imaging      = None
                        )
                    ),
                    selection(void"")
                  )
              ).void

            // Also update the calibrations science-role ETM
            def modeUpdate(modeType: ObservingModeType): F[Unit] =
              props.wavelengthAt.traverse_ : w =>
                val etm = ExposureTimeMode.SignalToNoiseMode(SignalToNoise.unsafeFromBigDecimalExact(100.0), w)
                services.observationService.updateObservations(
                  Services.asSuperUser:
                    AccessControl.unchecked(
                      ObservationPropertiesInput.Edit.Empty.copy(
                        observingMode = Nullable.NonNull(gmosScienceEtmEdit(modeType, etm))
                      ),
                      selection(sql" AND o.c_observing_mode_type = $observing_mode_type".apply(modeType))
                    )
                ).void

            requirementUpdate *> modeUpdate(ObservingModeType.GmosNorthLongSlit) *> modeUpdate(ObservingModeType.GmosSouthLongSlit)

      // Spec-phot calibrations use a fixed time and count; rewrite any whose
      // requirement or science ETM differs, including those still on S/N.
      private def syncSpecPhotoExposureTimeModes(
        calibrations: List[(ObsExtract[CalibrationConfigSubset], CalObsProps)]
      )(using Transaction[F]): F[Unit] =
        val expected: List[(Observation.Id, ObservingModeType, ExposureTimeMode)] =
          calibrations.flatMap: (o, props) =>
            gmosModeType(o.data).map: (modeType, config) =>
              (o.id, modeType, SpecPhotoExposureTime.forConfig(config, props.wavelengthAt))

        val roles = List(ExposureTimeModeRole.Requirement, ExposureTimeModeRole.Science)

        def oidSelection(oids: List[Observation.Id]): AppliedFragment =
          void"SELECT c_observation_id FROM t_observation WHERE c_observation_id IN (" |+|
            oids.map(sql"$observation_id").intercalate(void", ") |+| void")"

        NonEmptyList.fromList(expected.map(_._1)).traverse_ : oids =>
          services.exposureTimeModeService.select(oids.toList, roles*).flatMap: current =>
            val stale = expected.filterNot: (oid, _, etm) =>
              roles.forall(r => current.get(oid).flatMap(_.get(r)).exists(_.forall(_ === etm)))
            stale.groupBy((_, modeType, etm) => (modeType, etm)).toList.traverse_ : (key, entries) =>
              val (modeType, etm) = key
              val selection       = oidSelection(entries.map(_._1))
              def update(edit: ObservationPropertiesInput.Edit): F[Unit] =
                services.observationService.updateObservations(
                  Services.asSuperUser:
                    AccessControl.unchecked(edit, selection)
                ).void
              update(
                ObservationPropertiesInput.Edit.Empty.copy(
                  scienceRequirements = ScienceRequirementsInput(
                    exposureTimeMode = Nullable.NonNull(etm),
                    spectroscopy     = SpectroscopyScienceRequirementsInput.Default.some,
                    imaging          = None
                  ).some
                )
              ) *> update(
                ObservationPropertiesInput.Edit.Empty.copy(
                  observingMode = Nullable.NonNull(gmosScienceEtmEdit(modeType, etm))
                )
              )

      private def gmosModeType(config: CalibrationConfigSubset): Option[(ObservingModeType, CalibrationConfigSubset.Gmos)] =
        config match
          case c: GmosNConfigs    => (ObservingModeType.GmosNorthLongSlit, c).some
          case c: GmosSConfigs    => (ObservingModeType.GmosSouthLongSlit, c).some
          case c: GmosNIfuConfigs => (ObservingModeType.GmosNorthIfu, c).some
          case c: GmosSIfuConfigs => (ObservingModeType.GmosSouthIfu, c).some
          case _                  => none

      private def deleteEmptyCalibrationGroup(pid: Program.Id)(using Transaction[F], ServiceAccess): F[Unit] =
        groupService.selectGroups(pid).flatMap:
          case GroupTree.Root(_, children) =>
            children.collectFirst {
              case GroupTree.Branch(groupId = gid, children = obs, name = Some(CalibrationsGroupName), system = true)
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

        val gmosCalibs = toConfigForCalibration(allCalibs).collect(ObsExtract.perProgramCalibrationFilter)

        for {
          // Keep 'defined' or 'ready' observations where obscalc has been calculated,
          // plus 'ongoing' and 'completed' ones so their calibrations aren't dropped
          activeGmosSci   <- onlyCalibrationRelevant(allSci, _.id)
          // unique GMOS configurations
          uniqueSci       = uniqueConfiguration(activeGmosSci)
          // Extract props from all science observations, normalized per calibration type.
          propsByRole     = PerProgramPerConfigCalibrationTypes
                              .map(role => role -> calObsProps(toConfigForCalibration(allSci), role)).toMap
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
          addedOids      <- generateGMOSLSCalibrations(pid, propsByRole, configsPerRole, gnTgt, gsTgt)
          _              <- (info"Program $pid added calibrations $addedOids").whenA(addedOids.nonEmpty)
          calibUpdates   <- prepareCalibrationUpdates(gmosCalibs, removedOids, propsByRole)
          (specPhot, others) = calibUpdates.partition(_._1.role.contains(CalibrationRole.SpectroPhotometric))
          // The spec-phot ETM is synced separately, so only its band goes through here
          bandAndWave    = others.map((o, p) => (o.id, p)) ++ specPhot.map((o, p) => (o.id, p.copy(wavelengthAt = none)))
          _              <- updatePropsAt(bandAndWave.filter((_, p) => p.band.isDefined || p.wavelengthAt.isDefined))
          _              <- syncSpecPhotoExposureTimeModes(specPhot)
          // Delete the calibration group if empty
          _              <- deleteEmptyCalibrationGroup(pid)
        } yield (addedOids, removedOids)
