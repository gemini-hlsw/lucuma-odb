// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Order.catsKernelOrderingForOrder
import cats.data.Nested
import cats.data.NonEmptyList
import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.Site
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.Parallax
import lucuma.core.math.ProperMotion
import lucuma.core.math.RadialVelocity
import lucuma.core.math.RightAscension
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Target
import lucuma.core.util.Timestamp
import lucuma.odb.Config
import lucuma.odb.data.Existence
import lucuma.odb.data.GroupTree
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.input.CreateGroupInput
import lucuma.odb.graphql.input.EditAsterismsPatchInput
import lucuma.odb.graphql.input.GroupPropertiesInput
import lucuma.odb.graphql.input.ObservationPropertiesInput
import lucuma.odb.graphql.input.ScienceRequirementsInput
import lucuma.odb.graphql.input.SpectroscopyScienceRequirementsInput
import lucuma.odb.graphql.mapping.AccessControl
import lucuma.odb.sequence.ObservingMode
import lucuma.odb.sequence.data.GeneratorParams
import lucuma.odb.sequence.data.ItcInput
import lucuma.odb.service.CalibrationConfigSubset.*
import lucuma.odb.service.Services.ServiceAccess
import lucuma.odb.service.Services.Syntax.*
import lucuma.odb.util.Codecs.*
import lucuma.refined.*
import org.http4s.client.Client
import org.typelevel.log4cats.Logger
import skunk.AppliedFragment
import skunk.Command
import skunk.Query
import skunk.Transaction
import skunk.codec.numeric.int8
import skunk.codec.text.text
import skunk.syntax.all.*

import java.time.Instant

trait CalibrationsService[F[_]] {
  def setCalibrationRole(
    oid:  Observation.Id,
    role: Option[CalibrationRole]
  )(using Transaction[F], ServiceAccess): F[Unit]

  /**
    * Recalculates the calibrations for a program
    *
    * @param pid Program.Id
    * @param referenceInstant time used to calculate targets
    * @return list of added and removed calibration observations
    */
  def recalculateCalibrations(
    pid: Program.Id,
    referenceInstant: Instant
  )(using Transaction[F], ServiceAccess): F[(List[Observation.Id], List[Observation.Id])]

  /**
    * Returns the calibration targets for a given role adjusted to a reference instant
    */
  def calibrationTargets(roles: List[CalibrationRole], referenceInstant: Instant): F[List[(Target.Id, String, CalibrationRole, Coordinates)]]

  def recalculateCalibrationTarget(
    pid: Program.Id,
    oid: Observation.Id,
  )(using Transaction[F], ServiceAccess): F[Unit]
}

object CalibrationsService extends CalibrationObservations {
  val CalibrationsGroupName: NonEmptyString = "Calibrations".refined
  val CalibrationTypes = List(CalibrationRole.SpectroPhotometric, CalibrationRole.Twilight)

  case class ObsExtract[A](
    id:   Observation.Id,
    itc:  Option[ItcInput],
    band: Option[ScienceBand],
    role: Option[CalibrationRole],
    data: A
  ):
    def map[B](f: A => B): ObsExtract[B] =
      copy(data = f(data))

  case class CalObsProps(
    wavelengthAt: Option[Wavelength],
    band:         Option[ScienceBand]
  )

  private def targetCoordinates(when: Instant)(
    rows: List[(Target.Id, String, CalibrationRole, RightAscension, Declination, Epoch, Option[Long], Option[Long], Option[RadialVelocity], Option[Parallax])]
  ): List[(Target.Id, String, CalibrationRole, Coordinates)] =
    rows.map { case (tid, name, role, ra, dec, epoch, pmra, pmdec, rv, parallax) =>
      (tid,
        name,
        role,
        SiderealTracking(
          Coordinates(ra, dec),
          epoch,
          (pmra, pmdec).mapN{ case (r, d) =>
            ProperMotion(ProperMotion.μasyRA(r), ProperMotion.μasyDec(d))
          },
          rv,
          parallax
        ).at(when)
      )
    }.collect {
      case (tid, name, role, Some(st)) => (tid, name, role, st)
    }

  def instantiate[F[_]: {MonadCancelThrow, Services, Logger as L}](emailConfig: Config.Email, httpClient: Client[F]): CalibrationsService[F] =
    new CalibrationsService[F] {

      override def setCalibrationRole(
        oid:  Observation.Id,
        role: Option[CalibrationRole]
      )(using Transaction[F], ServiceAccess): F[Unit] =
        session.execute(Statements.SetCalibrationRole)(oid, role).void

      private def calibrationsGroup(pid: Program.Id, size: Int)(using Transaction[F]): F[Option[Group.Id]] =
        if (size > 0) {
          groupService(emailConfig, httpClient).selectGroups(pid).flatMap {
            case GroupTree.Root(_, c) =>
              val existing = c.collectFirst {
                case GroupTree.Branch(gid, _, _, _, Some(CalibrationsGroupName), _, _, _, true) => gid
              }
              // Create a system group for calibrations if it does not exist
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

      private def collectValid(
        requiresItcInputs: Boolean
      ): PartialFunction[(Observation.Id, Either[GeneratorParamsService.Error, GeneratorParams]), ObsExtract[ObservingMode]] =
        {
          case (oid, Right(GeneratorParams(itc, band, mode, calibRole, _, _))) if itc.isRight || !requiresItcInputs =>
            ObsExtract(oid, itc.toOption, band, calibRole, mode)
        }

      // Find all active observations in the program
      private def activeObservations(pid: Program.Id): F[Set[Observation.Id]] =
        session.execute(Statements.selectActiveObservations)(pid).map(_.toSet)

      /**
       * Requests configuration params for a program id and selection (either science or calibration)
       */
      private def allObservations(
        pid:       Program.Id,
        selection: ObservationSelection
      )(using Transaction[F]): F[List[ObsExtract[ObservingMode]]] =
        services
          .generatorParamsService
          .selectAll(pid, selection = selection)
          .map(_.toList.collect(collectValid(selection === ObservationSelection.Science)))

      private def allUnexecutedObservations(
        pid:       Program.Id,
        selection: ObservationSelection
      )(using Transaction[F]): F[List[ObsExtract[ObservingMode]]] =
        services
          .generatorParamsService
          .selectAllUnexecuted(pid, selection = selection)
          .map(_.toList.collect(collectValid(selection === ObservationSelection.Science)))

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
              // there must be a way to sum in wavelength space :/
              val pm = ws.map(_.toPicometers.value.value).combineAll / ws.size
              PosInt.from(pm).map(Wavelength(_)).toOption
          k -> CalObsProps(w, v.map(_.band).min)
        }

      private def uniqueConfiguration(
        all: List[ObsExtract[ObservingMode]]
      ): List[CalibrationConfigSubset] = all.map(_.data.toConfigSubset).distinct


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

      // Set the calibration role of the observations in bulk
      private def setCalibRoleAndGroup(oids: List[Observation.Id], calibrationRole: CalibrationRole): F[Unit] =
        session.executeCommand(Statements.setCalibRole(oids, calibrationRole)).void

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
        // Determine which calibrations are unnecessary
        val unnecessaryOids = calibrations.collect {
          case ObsExtract(oid, _, _, Some(role), config)
          if (!isCalibrationNeeded(scienceConfigs, config, role)) =>
            oid
        }

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
            val bandFragment = props.band.map(sql"o.c_science_band IS DISTINCT FROM $science_band")
            val waveFragment = props.wavelengthAt.map(sql"e.c_signal_to_noise_at <> $wavelength_pm")
            val needsUpdate  = List(bandFragment, waveFragment).flatten.intercalate(void" OR ")

            val etmJoin: AppliedFragment =
              if props.wavelengthAt.isDefined then
                void"""
                  LEFT JOIN t_exposure_time_mode_link k USING (c_observation_id)
                  LEFT JOIN t_exposure_time_mode e ON e.c_exposure_time_mode_id = r.c_exposure_time_mode_id
                """
              else
                void""

            services.observationService.updateObservations(
              Services.asSuperUser:
                AccessControl.unchecked(
                  ObservationPropertiesInput.Edit.Empty.copy(
                    scienceBand         = Nullable.orAbsent(props.band),
                    scienceRequirements = props.wavelengthAt.map: w =>
                      ScienceRequirementsInput(
                        exposureTimeMode = Nullable.NonNull(
                          ExposureTimeMode.SignalToNoiseMode(
                            SignalToNoise.unsafeFromBigDecimalExact(100.0),
                            w
                          )
                        ),
                        spectroscopy = SpectroscopyScienceRequirementsInput.Default.some,
                        imaging      = None
                      )
                  ),
                  // Important: Only update the obs that need it or it will produce a cascade of infinite updates
                  // TODO: This could be slightly optimized by grouping obs per configuration and updating in batches
                  void"""
                    SELECT c_observation_id
                      FROM t_observation o
                  """ |+| etmJoin |+| sql"""
                    WHERE o.c_observation_id = $observation_id
                      AND o.c_calibration_role IS NOT NULL
                      AND (
                  """.apply(oid) |+| needsUpdate |+| void")"
                )
            )
          }.void

      override def calibrationTargets(roles: List[CalibrationRole], referenceInstant: Instant)
        : F[List[(Target.Id, String, CalibrationRole, Coordinates)]] =
          session.execute(Statements.selectCalibrationTargets(roles))(roles)
            .map(targetCoordinates(referenceInstant))

      def recalculateCalibrations(pid: Program.Id, referenceInstant: Instant)(using Transaction[F], ServiceAccess): F[(List[Observation.Id], List[Observation.Id])] =
        for {
          _            <- L.info(s"Recalculating calibrations for $pid, reference instant  $referenceInstant")
          // Read calibration targets
          tgts         <- calibrationTargets(CalibrationTypes, referenceInstant)
          // Actual target for GN and GS
          gsTgt         = CalibrationIdealTargets(Site.GS, referenceInstant, tgts)
          gnTgt         = CalibrationIdealTargets(Site.GN, referenceInstant, tgts)
          // List of the program's active observations
          active       <- activeObservations(pid)
          _            <- L.debug(s"Program $pid has ${active.size} active observations: $active").whenA(active.nonEmpty)
          // Get all the active science observations
          allSci       <- allObservations(pid, ObservationSelection.Science)
                            .map(_.filter(u => active.contains(u._1)))
          _            <- L.debug(s"Program $pid has ${allSci.length} science observations: ${allSci.map(_.id)}").whenA(allSci.nonEmpty)
          // Unique science configurations
          uniqueSci     = uniqueConfiguration(allSci)
          _            <- L.debug(s"Program $pid has ${uniqueSci.length} science configurations")
          // Get all the active calibration observations (excluding those with execution events)
          allCalibs    <- allUnexecutedObservations(pid, ObservationSelection.Calibration).map(_.filter(u => active.contains(u.id)))
          _            <- L.debug(s"Program $pid has ${allCalibs.length} active calibration observations: ${allCalibs.map(_.id)}").whenA(allCalibs.nonEmpty)
          calibs        = toConfigForCalibration(allCalibs)
          // Average s/n wavelength at each configuration
          props         = calObsProps(toConfigForCalibration(allSci))
          // Per-calibration-type configuration processing
          configsPerRole = calculateConfigurationsPerRole(uniqueSci, calibs)
          // Remove calibrations that are not needed, basically when a config is removed
          removedOids  <- removeUnnecessaryCalibrations(uniqueSci, calibs)
          _            <- L.debug(s"Program $pid will remove unnecessary calibrations $removedOids").whenA(removedOids.nonEmpty)
          // Generate new calibrations for each unique configuration
          addedOids    <- generateGMOSLSCalibrations(pid, props, configsPerRole, gnTgt, gsTgt)
          _            <- L.debug(s"Program $pid added calibrations $addedOids").whenA(addedOids.nonEmpty)
          // Update wavelength at for each remaining calib
          calibUpdates  = prepareCalibrationUpdates(calibs, removedOids, props)
          _            <- updatePropsAt(calibUpdates)
          _            <- targetService.deleteOrphanCalibrationTargets(pid)
        } yield (addedOids, removedOids)

      // Recalcula the target of a calibration observation
      def recalculateCalibrationTarget(
        pid: Program.Id,
        oid: Observation.Id,
      )(using Transaction[F], ServiceAccess): F[Unit] = {
        for {
          _    <- L.info(s"Recalculating calibration targets for $pid, oid $oid")
          o    <- session.execute(Statements.selectCalibrationTimeAndConf)(oid).map(_.headOption)
          // Find the original target
          otgs <- o.map(_._1).map { oid =>
                    Services.asSuperUser:
                      asterismService.getAsterism(pid, oid).map(_.map(_._1))
                  }.getOrElse(List.empty.pure[F])
          // Select a new target
          tgts <- o match {
                  case Some(oid, cr, Some(ot), Some(ObservingModeType.GmosNorthLongSlit)) =>
                    session
                      .execute(Statements.selectCalibrationTargets(CalibrationTypes))(CalibrationTypes)
                      .map(targetCoordinates(ot.toInstant).map(CalibrationIdealTargets(Site.GN, ot.toInstant, _)).map(_.bestTarget(cr)))
                  case Some(oid, cr, Some(ot), Some(ObservingModeType.GmosSouthLongSlit)) =>
                    session
                      .execute(Statements.selectCalibrationTargets(CalibrationTypes))(CalibrationTypes)
                      .map(targetCoordinates(ot.toInstant).map(CalibrationIdealTargets(Site.GS, ot.toInstant, _)).map(_.bestTarget(cr)))
                  case _ =>
                    none.pure[F]
               }
            // Update the target on the calibration
            _ <- (o.map(_._1), tgts).mapN { (oid, tgtid) =>
                    for {
                      ct <- Nested(targetService.cloneTargetInto(tgtid, pid)).map(_._2).value
                      _  <- ct.traverse(ct => asterismService
                              .updateAsterism(
                                Services.asSuperUser:
                                  AccessControl.unchecked(
                                    EditAsterismsPatchInput(
                                      Some(List(ct)),
                                      NonEmptyList.fromList(otgs).map(_.toList)
                                    ),
                                    List(oid),
                                    observation_id
                                  )
                              )
                            )
                    } yield ()
                }.getOrElse(Result.unit.pure[F])
            // targets may have been orphaned, delete those
            _  <- targetService.deleteOrphanCalibrationTargets(pid)
        } yield ()
      }
    }

  object Statements {

    val SetCalibrationRole: Command[(Observation.Id, Option[CalibrationRole])] =
      sql"""
        UPDATE t_observation
        SET c_calibration_role = ${calibration_role.opt}
        WHERE c_observation_id = $observation_id
      """.command.contramap((a, b) => (b, a))

    def setCalibRole(oids: List[Observation.Id], role: CalibrationRole): AppliedFragment =
      void"UPDATE t_observation " |+|
        sql"SET c_calibration_role = $calibration_role "(role) |+|
        void"WHERE c_observation_id IN (" |+|
          oids.map(sql"$observation_id").intercalate(void", ") |+| void")"

    def selectCalibrationTargets(roles: List[CalibrationRole]): Query[List[CalibrationRole], (Target.Id, String, CalibrationRole, RightAscension, Declination, Epoch, Option[Long], Option[Long], Option[RadialVelocity], Option[Parallax])] =
      sql"""SELECT
              c_target_id,
              t_target.c_name,
              t_target.c_calibration_role,
              c_sid_ra,
              c_sid_dec,
              c_sid_epoch,
              c_sid_pm_ra,
              c_sid_pm_dec,
              c_sid_rv,
              c_sid_parallax
            FROM t_target
            INNER JOIN t_program
            ON t_target.c_program_id=t_program.c_program_id
            WHERE t_program.c_calibration_role IN(${calibration_role.list(roles.length)})
              AND t_program.c_existence='present'
              AND t_target.c_existence='present'
              AND t_target.c_target_disposition = 'calibration'
            ORDER BY c_target_id
          """.query(target_id *: text *: calibration_role *: right_ascension *: declination *: epoch *: int8.opt *: int8.opt *: radial_velocity.opt *: parallax.opt)

    val selectCalibrationTimeAndConf: Query[Observation.Id, (Observation.Id, CalibrationRole, Option[Timestamp], Option[ObservingModeType])] =
      sql"""
        SELECT
            c_observation_id,
            c_calibration_role,
            c_observation_time,
            c_observing_mode_type
          FROM t_observation
          WHERE c_observation_id = $observation_id AND c_calibration_role IS NOT NULL
          """.query(observation_id *: calibration_role *: core_timestamp.opt *: observing_mode_type.opt)

    val selectActiveObservations: Query[Program.Id, Observation.Id] =
      sql"""
        SELECT
            c_observation_id
          FROM t_observation
          WHERE c_program_id = $program_id AND c_workflow_user_state IS DISTINCT FROM 'inactive'
          """.query(observation_id)

  }
}
