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
import lucuma.core.enums.ProgramType
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.Site
import lucuma.core.math.Coordinates
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.Attachment
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.odb.data.Existence
import lucuma.odb.data.ExposureTimeModeRole
import lucuma.odb.data.GroupTree
import lucuma.odb.data.Nullable
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
import lucuma.odb.service.Services.ServiceAccess
import lucuma.odb.service.Services.Syntax.*
import lucuma.odb.util.Codecs.*
import lucuma.refined.*
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.syntax.*
import org.typelevel.otel4s.trace.Tracer
import skunk.AppliedFragment
import skunk.Command
import skunk.Query
import skunk.Transaction
import skunk.codec.all.text
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
    new PerProgramPerConfigCalibrationsService[F] with CalibrationObservations with ObsCalibrationOperations with WorkflowStateQueries[F]:
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
                              minimumObservationsPerNight = none,
                              maximumObservationsPerNight = none,
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
                    calibrationRoles = List(CalibrationRole.Twilight, CalibrationRole.SpectroPhotometric, CalibrationRole.MosMaskFlat)
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
        ObsExtract.PerProgramPerConfigCalibrationTypes.map { calibType =>
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
        pid:            Program.Id,
        scienceConfigs: List[CalibrationConfigSubset],
        calibrations:   List[ObsExtract[CalibrationConfigSubset]]
      )(using Transaction[F], ServiceAccess): F[List[Observation.Id]] = {
        val candidates = calibrations.collect {
          case ObsExtract(id = oid, role = Some(role), data = config)
            if !isCalibrationNeeded(scienceConfigs, config, role) => oid
        }

        for
          unnecessary <- excludeOngoingAndCompleted(candidates, identity)
          specPhots   <- selectSpecPhots(pid).map(_.filter(sp => unnecessary.contains(sp.oid)))
          // An arc that has executed or been visited keeps its standard and group.
          arcs        <- excludeObsCalibrationsFromDeletion(specPhots.flatMap(_.arc), identity)
          kept         = specPhots.filter(_.arc.exists(a => !arcs.contains(a)))
          toDelete     = unnecessary.filterNot(kept.map(_.oid).contains)
          // The arc leaves the group before its standard does, as the group trigger requires.
          _           <- NonEmptyList.fromList(arcs).traverse_(observationService.deleteCalibrationObservations)
          _           <- NonEmptyList.fromList(toDelete).traverse_(observationService.deleteCalibrationObservations)
          _           <- specPhots.diff(kept).flatMap(_.group).traverse_(groupService.deleteSystemGroup(pid, _))
        yield toDelete ++ arcs
      }

      // A spectrophotometric standard with the system group that holds it and its
      // daytime arc, once they exist.
      private case class SpecPhot(
        oid:      Observation.Id,
        modeType: Option[ObservingModeType],
        group:    Option[Group.Id],
        arc:      Option[Observation.Id]
      )

      private def selectSpecPhots(pid: Program.Id): F[List[SpecPhot]] =
        session.execute(Statements.selectSpecPhots)(pid).map(_.map(SpecPhot.apply))

      private def newSpecPhotGroup(pid: Program.Id, parent: Group.Id, sp: SpecPhot)(using Transaction[F]): F[Group.Id] =
        newObsCalibrationGroup(
          pid,
          NonEmptyString.unsafeFrom(s"${sp.modeType.fold("gmos")(_.tag)}/calibration/${sp.oid.show}"),
          parent.some,
          none,
          none,
          List(CalibrationRole.DaytimeArc),
          sp.oid
        )

      // The arc carries a copy of its standard's observing mode, exposure time
      // modes included, and the standard's band and constraints.
      private def syncArcConfiguration(sp: SpecPhot, arcOid: Observation.Id)(using Transaction[F]): F[Unit] =
        syncObservationConfiguration(sp.oid, arcOid) *>
          selectObservingModeTypes(List(arcOid)).flatMap: modes =>
            replaceObservingMode(sp.oid, sp.modeType, arcOid, modes.get(arcOid).flatten)

      // Every unexecuted standard gets its group, nested in the Calibrations
      // group, and one daytime arc kept in step with it unless the arc has
      // executed.  An executed standard is left as it is, arc or no arc.
      private def syncDaytimeArcs(pid: Program.Id)(using Transaction[F], ServiceAccess): F[List[Observation.Id]] =
        def sync(parent: Group.Id, sp: SpecPhot): F[Option[Observation.Id]] =
          for
            gid <- sp.group.fold(newSpecPhotGroup(pid, parent, sp))(_.pure[F])
            res <- sp.arc match
                     case None      =>
                       insertDaytimeCalibration(pid, gid, CalibrationRole.DaytimeArc)
                         .flatTap(syncArcConfiguration(sp, _))
                         .map(_.some)
                     case Some(arc) =>
                       excludeOngoingAndCompleted(List(arc), identity)
                         .flatMap(_.traverse_(syncArcConfiguration(sp, _)))
                         .as(none[Observation.Id])
          yield res

        for
          specPhots <- selectSpecPhots(pid).flatMap(excludeOngoingAndCompleted(_, _.oid))
          parent    <- calibrationsGroup(pid, specPhots.size)
          added     <- parent.fold(List.empty[Observation.Id].pure[F]): cg =>
                         specPhots.traverse(sync(cg, _)).map(_.flatten)
          _         <- (info"Program $pid added daytime arcs $added").whenA(added.nonEmpty)
        yield added

      // A standard's group left without its standard loses its arcs, unless
      // one has executed, and then the group itself.
      private def cleanupOrphanedSpecPhotGroups(pid: Program.Id)(using Transaction[F], ServiceAccess): F[List[Observation.Id]] =
        session.execute(Statements.selectOrphanedSpecPhotGroups)(pid).flatMap: groups =>
          groups.flatTraverse: (gid, arcs) =>
            for
              deletable <- excludeObsCalibrationsFromDeletion(arcs, identity)
              _         <- NonEmptyList.fromList(deletable).traverse_(observationService.deleteCalibrationObservations)
              _         <- groupService.deleteSystemGroup(pid, gid).whenA(deletable.length === arcs.length)
            yield deletable

      // A MOS science observation using a mask, with the file the mask holds now.
      private case class MaskUse(
        attachment: Attachment.Id,
        oid:        Observation.Id,
        modeType:   ObservingModeType,
        band:       Option[ScienceBand],
        remotePath: String
      )

      // A present MOS mask flat, the mask its mode holds and the file it imaged.
      private case class MaskFlat(
        oid:        Observation.Id,
        attachment: Option[Attachment.Id],
        remotePath: Option[String]
      )

      private def recordMaskFile(oid: Observation.Id, remotePath: String): F[Unit] =
        session.execute(Statements.upsertMosMaskFlatFile)((oid, remotePath)).void

      private def setBand(oid: Observation.Id, band: Option[ScienceBand]): F[Unit] =
        session.execute(Statements.updateBand)((band, oid)).void

      // One flat per mask in use, copying the mode of the lowest-id observation
      // using it; an untaken flat recopies it on every pass.  A flat taken of a
      // replaced file is kept and a new one made; an untaken flat simply follows
      // the new file.
      private def syncMosMaskFlats(pid: Program.Id)(using Transaction[F], ServiceAccess): F[(List[Observation.Id], List[Observation.Id])] =
        def sync(cg: Group.Id, flats: List[MaskFlat], untaken: Set[Observation.Id], uses: NonEmptyList[MaskUse]): F[Option[Observation.Id]] =
          val source = uses.minimumBy(_.oid)
          val band   = uses.toList.flatMap(_.band).minOption
          val mine   = flats.filter(_.attachment.contains(source.attachment))
          def refresh(oid: Observation.Id): F[Unit] =
            for
              modes <- selectObservingModeTypes(List(oid))
              _     <- replaceObservingMode(source.oid, source.modeType.some, oid, modes.get(oid).flatten)
              _     <- setBand(oid, band)
            yield ()
          mine.find(_.remotePath.contains(source.remotePath)) match
            case Some(f) =>
              refresh(f.oid).whenA(untaken.contains(f.oid)).as(none)
            case None    =>
              mine.find(f => untaken.contains(f.oid)) match
                case Some(f) =>
                  (recordMaskFile(f.oid, source.remotePath) *> refresh(f.oid)).as(none)
                case None    =>
                  for
                    oid <- insertDaytimeCalibration(pid, cg, CalibrationRole.MosMaskFlat)
                    _   <- replaceObservingMode(source.oid, source.modeType.some, oid, none)
                    _   <- recordMaskFile(oid, source.remotePath)
                    _   <- setBand(oid, band)
                  yield oid.some

        session.unique(Statements.selectProgramType)(pid).flatMap:
          case ProgramType.Calibration =>
            (List.empty[Observation.Id], List.empty[Observation.Id]).pure[F]
          case _                       =>
            for
              uses      <- session.execute(Statements.selectMaskUses)(pid).map(_.map(MaskUse.apply))
              byMask     = uses.groupByNel(_.attachment)
              flats     <- session.execute(Statements.selectMosMaskFlats)(pid).map(_.map(MaskFlat.apply))
              unused     = flats.filterNot(_.attachment.exists(byMask.contains))
              removable <- excludeOngoingAndCompleted(unused, _.oid).map(_.map(_.oid))
              _         <- NonEmptyList.fromList(removable).traverse_(observationService.deleteCalibrationObservations)
              untaken   <- excludeOngoingAndCompleted(flats.filterNot(f => removable.contains(f.oid)), _.oid).map(_.map(_.oid).toSet)
              parent    <- calibrationsGroup(pid, byMask.size)
              added     <- parent.fold(List.empty[Observation.Id].pure[F]): cg =>
                             byMask.values.toList.traverse(sync(cg, flats, untaken, _)).map(_.flatten)
              _         <- (info"Program $pid added MOS mask flats $added").whenA(added.nonEmpty)
              _         <- (info"Program $pid removed MOS mask flats $removable").whenA(removable.nonEmpty)
            yield (added, removable)

      // Update the signal to noise at wavelength for each calbiration observation depending
      // on the average wavelength of the configuration.  The spec-phot ETM is a fixed time
      // and count, so only its band follows the science.
      private def prepareCalibrationUpdates(
        calibrations: List[ObsExtract[CalibrationConfigSubset]],
        removedOids:  List[Observation.Id],
        propsByRole:  Map[CalibrationRole, Map[CalibrationConfigSubset, CalObsProps]]
      ): F[List[(Observation.Id, CalObsProps)]] =
        val candidates =
          calibrations
            .filterNot { o => removedOids.contains(o.id) }
            .map { o => (o.id, o.role.flatMap(calibrationRole =>
              propsByRole.get(calibrationRole).flatMap(_.get(o.data)).map: props =>
                if calibrationRole === CalibrationRole.SpectroPhotometric then props.copy(wavelengthAt = none) else props
            )) }
            .collect { case (oid, Some(props)) if props.band.isDefined || props.wavelengthAt.isDefined => (oid, props) }
        excludeOngoingAndCompleted(candidates, _._1)

      // A partial GMOS Long Slit mode edit that touches only the science ETM
      private def gmosLongSlitScienceEtmEdit(modeType: ObservingModeType, etm: ExposureTimeMode): ObservingModeInput.Edit =
        val common = GmosLongSlitInput.Edit.Common.AllUndefined.copy(exposureTimeMode = etm.some)
        val (gn, gs) = modeType match
          case ObservingModeType.GmosNorthLongSlit =>
            (GmosLongSlitInput.Edit.North(none, Nullable.Absent, none, common, none).some, none)
          case ObservingModeType.GmosSouthLongSlit =>
            (none, GmosLongSlitInput.Edit.South(none, Nullable.Absent, none, common, none).some)
          case other =>
            sys.error(s"gmosLongSlitScienceEtmEdit: unexpected observing mode type $other")
        ObservingModeInput.Edit(
          daytimeCalibration = none,
          exchange           = none,
          flamingos2Imaging  = none,
          flamingos2LongSlit = none,
          flamingos2Mos      = none,
          ghostIfu           = none,
          gmosNorthIfu       = none,
          gmosNorthImaging   = none,
          gmosNorthLongSlit  = gn,
          gmosNorthMos       = none,
          gmosSouthIfu       = none,
          gmosSouthImaging   = none,
          gmosSouthLongSlit  = gs,
          gmosSouthMos       = none,
          gnirsImaging       = none,
          gnirsSpectroscopy  = none,
          igrins2LongSlit    = none,
          visitor            = none
        )

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
                        observingMode = Nullable.NonNull(gmosLongSlitScienceEtmEdit(modeType, etm))
                      ),
                      selection(sql" AND o.c_observing_mode_type = $observing_mode_type".apply(modeType))
                    )
                ).void

            requirementUpdate *> modeUpdate(ObservingModeType.GmosNorthLongSlit) *> modeUpdate(ObservingModeType.GmosSouthLongSlit)

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
          propsByRole     = ObsExtract.PerProgramPerConfigCalibrationTypes
                              .map(role => role -> calObsProps(toConfigForCalibration(allSci), role)).toMap
          // Create ideal targets for each site
          gnTgt           = CalibrationIdealTargets(Site.GN, when, calibTargets)
          gsTgt           = CalibrationIdealTargets(Site.GS, when, calibTargets)
          configsPerRole  = calculateConfigurationsPerRole(uniqueSci, gmosCalibs)
          _              <- info"===== Recalculating shared calibrations for program ID: $pid, instant $when ====="
          _              <- info"Program $pid has ${uniqueSci.length} science configurations"
          // Remove calibrations that are not needed, basically when a config is removed
          removedOids    <- removeUnnecessaryCalibrations(pid, uniqueSci, gmosCalibs)
          _              <- (info"Program $pid will remove unnecessary calibrations $removedOids").whenA(removedOids.nonEmpty)
          // Generate new calibrations for each unique configuration
          addedOids      <- generateGMOSLSCalibrations(pid, propsByRole, configsPerRole, gnTgt, gsTgt)
          _              <- (info"Program $pid added calibrations $addedOids").whenA(addedOids.nonEmpty)
          calibUpdates   <- prepareCalibrationUpdates(gmosCalibs, removedOids, propsByRole)
          _              <- updatePropsAt(calibUpdates)
          // Each standard keeps a daytime arc in a group of its own, synced
          // once the standard's band is final
          orphanedArcs   <- cleanupOrphanedSpecPhotGroups(pid)
          addedArcs      <- syncDaytimeArcs(pid)
          (addedFlats, removedFlats) <- syncMosMaskFlats(pid)
          // Delete the calibration group if empty
          _              <- deleteEmptyCalibrationGroup(pid)
        } yield (addedOids ++ addedArcs ++ addedFlats, removedOids ++ orphanedArcs ++ removedFlats)

  object Statements:

    // Present standards with the arc group that holds them and its present arc, if any.
    val selectSpecPhots: Query[Program.Id, (Observation.Id, Option[ObservingModeType], Option[Group.Id], Option[Observation.Id])] =
      sql"""
        SELECT s.c_observation_id, s.c_observing_mode_type, g.c_group_id, a.c_observation_id
        FROM   t_observation s
        LEFT JOIN t_group g
          ON  g.c_group_id = s.c_group_id
          AND g.c_system
          AND 'daytime_arc' = ANY (g.c_calibration_roles)
        LEFT JOIN t_observation a
          ON  a.c_group_id         = g.c_group_id
          AND a.c_calibration_role = 'daytime_arc'
          AND a.c_existence        = 'present'
        WHERE s.c_program_id       = $program_id
          AND s.c_calibration_role = 'spectrophotometric'
          AND s.c_existence        = 'present'
        ORDER BY s.c_observation_id
      """.query(observation_id *: observing_mode_type.opt *: group_id.opt *: observation_id.opt)

    // Arc groups whose standard is gone, with the present arcs they still hold.
    val selectOrphanedSpecPhotGroups: Query[Program.Id, (Group.Id, List[Observation.Id])] =
      sql"""
        SELECT g.c_group_id,
               ARRAY_REMOVE(ARRAY_AGG(a.c_observation_id::varchar), NULL)
        FROM   t_group g
        LEFT JOIN t_observation a
          ON  a.c_group_id         = g.c_group_id
          AND a.c_calibration_role = 'daytime_arc'
          AND a.c_existence        = 'present'
        WHERE g.c_program_id = $program_id
          AND g.c_system
          AND g.c_existence = 'present'
          AND 'daytime_arc' = ANY (g.c_calibration_roles)
          AND NOT EXISTS (
            SELECT 1
            FROM   t_observation s
            WHERE  s.c_group_id         = g.c_group_id
              AND  s.c_calibration_role = 'spectrophotometric'
              AND  s.c_existence        = 'present'
          )
        GROUP BY g.c_group_id
      """.query(group_id *: _gid[Observation.Id])

    val selectProgramType: Query[Program.Id, ProgramType] =
      sql"""
        SELECT c_program_type FROM t_program WHERE c_program_id = $program_id
      """.query(program_type)

    // Masks used by present, not inactive, science MOS observations.
    val selectMaskUses: Query[Program.Id, (Attachment.Id, Observation.Id, ObservingModeType, Option[ScienceBand], String)] =
      sql"""
        SELECT m.c_mask_attachment_id, o.c_observation_id, o.c_observing_mode_type, o.c_science_band, a.c_remote_path
        FROM (
          SELECT c_observation_id, c_mask_attachment_id FROM t_gmos_north_mos
          UNION ALL
          SELECT c_observation_id, c_mask_attachment_id FROM t_gmos_south_mos
          UNION ALL
          SELECT c_observation_id, c_mask_attachment_id FROM t_flamingos_2_mos
        ) m
        JOIN t_observation o ON o.c_observation_id = m.c_observation_id
        JOIN t_attachment  a ON a.c_attachment_id  = m.c_mask_attachment_id
        JOIN v_execution_state e ON e.c_observation_id = o.c_observation_id
        WHERE o.c_program_id       = $program_id
          AND o.c_calibration_role IS NULL
          AND o.c_existence        = 'present'
          AND o.c_workflow_user_state IS DISTINCT FROM 'inactive'
          AND e.c_execution_state NOT IN ('completed', 'declared_complete')
      """.query(attachment_id *: observation_id *: observing_mode_type *: science_band.opt *: text)

    // Present MOS mask flats with the mask their mode holds and the file they imaged.
    val selectMosMaskFlats: Query[Program.Id, (Observation.Id, Option[Attachment.Id], Option[String])] =
      sql"""
        SELECT o.c_observation_id, m.c_mask_attachment_id, f.c_remote_path
        FROM t_observation o
        LEFT JOIN (
          SELECT c_observation_id, c_mask_attachment_id FROM t_gmos_north_mos
          UNION ALL
          SELECT c_observation_id, c_mask_attachment_id FROM t_gmos_south_mos
          UNION ALL
          SELECT c_observation_id, c_mask_attachment_id FROM t_flamingos_2_mos
        ) m ON m.c_observation_id = o.c_observation_id
        LEFT JOIN t_mos_mask_flat f ON f.c_observation_id = o.c_observation_id
        WHERE o.c_program_id       = $program_id
          AND o.c_calibration_role = 'mos_mask_flat'
          AND o.c_existence        = 'present'
        ORDER BY o.c_observation_id
      """.query(observation_id *: attachment_id.opt *: text.opt)

    val upsertMosMaskFlatFile: Command[(Observation.Id, String)] =
      sql"""
        INSERT INTO t_mos_mask_flat (c_observation_id, c_remote_path)
        VALUES ($observation_id, $text)
        ON CONFLICT (c_observation_id) DO UPDATE SET c_remote_path = EXCLUDED.c_remote_path
      """.command

    val updateBand: Command[(Option[ScienceBand], Observation.Id)] =
      sql"""
        UPDATE t_observation SET c_science_band = ${science_band.opt} WHERE c_observation_id = $observation_id
      """.command
