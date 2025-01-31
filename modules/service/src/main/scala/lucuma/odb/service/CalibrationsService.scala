// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import lucuma.odb.data.Existence
import lucuma.odb.data.GroupTree
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.input.CreateGroupInput
import lucuma.odb.graphql.input.GroupPropertiesInput
import lucuma.odb.graphql.input.ObservationPropertiesInput
import lucuma.odb.graphql.input.ScienceRequirementsInput
import lucuma.odb.graphql.input.SpectroscopyScienceRequirementsInput
import lucuma.odb.sequence.ObservingMode
import lucuma.odb.sequence.data.GeneratorParams
import lucuma.odb.sequence.data.ItcInput
import lucuma.odb.service.CalibrationConfigSubset.*
import lucuma.odb.service.Services.Syntax.*
import lucuma.odb.util.*
import lucuma.odb.util.Codecs.*
import lucuma.refined.*
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
  )(using Transaction[F]): F[Unit]

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
  )(using Transaction[F]): F[(List[Observation.Id], List[Observation.Id])]

  /**
    * Returns the calibration targets for a given role adjusted to a reference instant
    */
  def calibrationTargets(roles: List[CalibrationRole], referenceInstant: Instant): F[List[(Target.Id, String, CalibrationRole, Coordinates)]]

  def recalculateCalibrationTarget(
    pid: Program.Id,
    oid: Observation.Id,
  )(using Transaction[F]): F[Unit]
}

object CalibrationsService extends CalibrationObservations {
  val CalibrationsGroupName: NonEmptyString = "Calibrations".refined
  val CalibrationTypes = List(CalibrationRole.SpectroPhotometric, CalibrationRole.Twilight)

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

  def instantiate[F[_]: MonadCancelThrow](using Services[F]): CalibrationsService[F] =
    new CalibrationsService[F] {

      override def setCalibrationRole(
        oid:  Observation.Id,
        role: Option[CalibrationRole]
      )(using Transaction[F]): F[Unit] =
        session.execute(Statements.SetCalibrationRole)(oid, role).void

      private def calibrationsGroup(pid: Program.Id, size: Int)(using Transaction[F]): F[Option[Group.Id]] =
        if (size > 0) {
          groupService.selectGroups(pid).flatMap {
            case GroupTree.Root(_, c) =>
              val existing = c.collectFirst {
                case GroupTree.Branch(gid, _, _, _, Some(CalibrationsGroupName), _, _, _, true) => gid
              }
              // Create a system group for calibrations if it does not exist
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
                    system = true
                  ).map(_.toOption)
              }
            case _ => none.pure[F]
          }
        } else none.pure[F]

      private def collectValid(
        requiresItcInputs: Boolean
      ): PartialFunction[(Observation.Id, Either[GeneratorParamsService.Error, GeneratorParams]), (Observation.Id, Option[ItcInput], ObservingMode)] =
        {
          case (oid, Right(GeneratorParams(itc, _, mode, _))) if itc.isRight || !requiresItcInputs => (oid, itc.toOption, mode)
        }

      // Find all active observations in the program
      private def activeObservations(pid: Program.Id)(using Transaction[F]): F[List[Observation.Id]] =
        session.execute(Statements.selectActiveObservations)(pid)

      /**
       * Requests configuration params for a program id and selection (either science or calibration)
       */
      private def allObservations(
        pid:       Program.Id,
        selection: ObservationSelection
      )(using Transaction[F]): F[List[(Observation.Id, Option[ItcInput], ObservingMode)]] =
        services
          .generatorParamsService
          .selectAll(pid, selection = selection)
          .map(_.toList.collect(collectValid(selection === ObservationSelection.Science)))

      private def toConfigForCalibration(all: List[(Observation.Id, Option[ItcInput], ObservingMode)]): List[(Observation.Id, Option[ItcInput], CalibrationConfigSubset)] =
        all.map(_.map(_.toConfigSubset))

      private def configAtWavelength(
        calibConfigs: List[(Observation.Id, Option[ItcInput], CalibrationConfigSubset)]
      ): Map[CalibrationConfigSubset, Option[Wavelength]] =
        calibConfigs.groupBy(_._3).map { case (k, v) =>
          val lw = v.map(_._2.map(_.spectroscopy.exposureTimeMode.at)).flattenOption
          val meanWv = if (lw.isEmpty) None
          else
            // there must be a way to sum in wavelength space :/
            val pm = lw.map(_.toPicometers.value.value).combineAll / lw.size
            PosInt.from(pm).map(Wavelength(_)).toOption
          k -> meanWv
        }

      private def uniqueConfiguration(
        all: List[(Observation.Id, Option[ItcInput], ObservingMode)]
      ): List[(Observation.Id, Option[ItcInput], CalibrationConfigSubset)] =
        val calibConfigs = toConfigForCalibration(all)
        calibConfigs.distinctBy(_._3)

      private def calibObservation(
        calibRole: CalibrationRole,
        site: Site,
        pid: Program.Id,
        gid: Group.Id,
        wvAt: Map[CalibrationConfigSubset, Option[Wavelength]],
        configs: List[CalibrationConfigSubset],
        tid: Target.Id
      )(using Transaction[F]): F[List[Observation.Id]] =
        calibRole match {
          case CalibrationRole.SpectroPhotometric =>
            site match {
              case Site.GN => gmosLongSlitSpecPhotObs(pid, gid, tid, wvAt, configs.collect { case c: GmosNConfigs => c })
              case Site.GS => gmosLongSlitSpecPhotObs(pid, gid, tid, wvAt, configs.collect { case c: GmosSConfigs => c })
            }
          case CalibrationRole.Twilight =>
            site match
              case Site.GN => gmosLongSlitTwilightObs(pid, gid, tid, configs.collect { case c: GmosNConfigs => c })
              case Site.GS => gmosLongSlitTwilightObs(pid, gid, tid, configs.collect { case c: GmosSConfigs => c })
          case _ => List.empty.pure[F]
        }

      private def gmosCalibrations(
        pid: Program.Id,
        gid: Group.Id,
        wvAt: Map[CalibrationConfigSubset, Option[Wavelength]],
        configs: List[CalibrationConfigSubset],
        gnCalc: CalibrationIdealTargets,
        gsCalc: CalibrationIdealTargets
      )(using Transaction[F]): F[List[(CalibrationRole, Observation.Id)]] = {
        def newCalibs(site: Site, ct: CalibrationRole, idealTarget: CalibrationIdealTargets): Option[F[List[(CalibrationRole, Observation.Id)]]] =
          idealTarget.bestTarget(ct).map(tgtid =>
            // We don't want to create a target if there are no pending configurations
            if (configs.nonEmpty) {
              (for {
                cta <- Nested(targetService.cloneTargetInto(tgtid, pid)).map(_._2).value
                o   <- cta.traverse(calibObservation(ct, site, pid, gid, wvAt, configs, _))
              } yield o).orError.map(_.map((ct, _)))
            } else {
              List.empty.pure[F]
            })

        CalibrationTypes.map { ct =>
          val gno = newCalibs(Site.GN, ct, gnCalc)
          val gso = newCalibs(Site.GS, ct, gsCalc)

          (gno, gso).mapN((_, _).mapN(_ ::: _)).getOrElse(List.empty.pure[F])
        }.sequence.map(_.flatten)
      }

      // Set the calibration role of the observations in bulk
      private def setCalibRoleAndGroup(oids: List[Observation.Id], calibrationRole: CalibrationRole)(using Transaction[F]): F[Unit] =
        session.executeCommand(Statements.setCalibRole(oids, calibrationRole)).void

      private def generateGMOSLSCalibrations(
        pid: Program.Id,
        wvAt: Map[CalibrationConfigSubset, Option[Wavelength]],
        configs: List[CalibrationConfigSubset],
        gnTgt: CalibrationIdealTargets,
        gsTgt: CalibrationIdealTargets
      )(using Transaction[F]): F[List[Observation.Id]] = {
        if (configs.isEmpty) {
          List.empty.pure[F]
        } else
          for {
            cg   <- calibrationsGroup(pid, configs.size)
            oids <- cg.map(g =>
                      gmosCalibrations(pid, g, wvAt, configs, gnTgt, gsTgt).flatTap { oids =>
                        oids.groupBy(_._1).toList.traverse { case (role, oids) =>
                          setCalibRoleAndGroup(oids.map(_._2), role).whenA(oids.nonEmpty)
                        }
                      }
                    ).getOrElse(List.empty.pure[F])
          } yield oids.map(_._2)
      }

      private def removeUnnecessaryCalibrations(
        scienceConfigs: List[CalibrationConfigSubset],
        calibrations:   List[(Observation.Id, CalibrationConfigSubset)]
      )(using Transaction[F]): F[List[Observation.Id]] = {
        val oids = NonEmptyList.fromList(
          calibrations
            .collect { case (oid, c) if !scienceConfigs.exists(_ === c) => oid }
            .sorted
        )
        oids.fold(List.empty.pure[F])(os => observationService.deleteCalibrationObservations(os).as(os.toList))
      }

      // Update the signal to noise at wavelength for each calbiration observation depending
      // on the average wavelength of the configuration
      private def updateWvAt(
        calibrations: List[(Observation.Id, Option[ItcInput], CalibrationConfigSubset)],
        removed: List[Observation.Id],
        atWavelength: Map[CalibrationConfigSubset, Option[Wavelength]],
        role: CalibrationRole
      )(using Transaction[F]): F[Unit] =
        calibrations
          .filterNot { case (oid, _, _) => removed.contains(oid) }
          .map { case (oid, _, c) => (oid, atWavelength.get(c).flatten) }
          .collect { case (oid, Some(w)) => (oid, w) }
          .traverse { (oid, w) =>
            services.observationService.updateObservations(
              ObservationPropertiesInput.Edit.Empty.copy(
                scienceRequirements = Some(
                  ScienceRequirementsInput(
                    mode         = None,
                    spectroscopy = Some(
                      SpectroscopyScienceRequirementsInput.Default.copy(
                        exposureTimeMode = Nullable.NonNull(
                          ExposureTimeMode.SignalToNoiseMode(
                            SignalToNoise.unsafeFromBigDecimalExact(100.0),
                            w
                          )
                        )
                      )
                    )
                  )
                )
              ),
              // Important: Only update the obs that need it or it will produce a cascade of infinite updates
              // TODO: This could be slightly optimized by grouping obs per configuration and updating in batches
              sql"select $observation_id where c_calibration_role = $calibration_role and c_spec_signal_to_noise_at <> $wavelength_pm".apply(oid, role, w)
            )
          }.void

      override def calibrationTargets(roles: List[CalibrationRole], referenceInstant: Instant)
        : F[List[(Target.Id, String, CalibrationRole, Coordinates)]] =
          session.execute(Statements.selectCalibrationTargets(roles))(roles)
            .map(targetCoordinates(referenceInstant))

      def recalculateCalibrations(pid: Program.Id, referenceInstant: Instant)(using Transaction[F]): F[(List[Observation.Id], List[Observation.Id])] =
        for
          // Read calibration targets
          tgts         <- calibrationTargets(CalibrationTypes, referenceInstant)
          // Actual target for GN and GS
          gsTgt         = CalibrationIdealTargets(Site.GS, referenceInstant, tgts)
          gnTgt         = CalibrationIdealTargets(Site.GN, referenceInstant, tgts)
          // List of the program's active observations
          active       <- activeObservations(pid)
          // Get all the active science observations
          allSci       <- allObservations(pid, ObservationSelection.Science)
                            .map(_.filter(u => active.contains(u._1)))
          // Unique science configurations
          uniqueSci     = uniqueConfiguration(allSci)
          // Get all the active calibration observations
          allCalibs    <- allObservations(pid, ObservationSelection.Calibration)
          // Unique calibration configurations
          uniqueCalibs  = uniqueConfiguration(allCalibs)
          calibs        = toConfigForCalibration(allCalibs)
          // Average s/n wavelength at each configuration
          configWvAt    = configAtWavelength(toConfigForCalibration(allSci))
          // Get all unique configurations
          configs       = uniqueSci.map(_._3).diff(uniqueCalibs.map(_._3))
          // Remove calibrations that are not needed, basically when a config is removed
          removedOids  <- removeUnnecessaryCalibrations(uniqueSci.map(_._3), calibs.map {
                            case (oid, _, c) => (oid, c)
                          })
          // Generate new calibrations for each unique configuration
          addedOids    <- generateGMOSLSCalibrations(pid, configWvAt, configs, gnTgt, gsTgt)
          // Update wavelength at for each remaining calib
          _            <- updateWvAt(calibs, removedOids, configWvAt, CalibrationRole.SpectroPhotometric)
          _            <- targetService.deleteOrphanCalibrationTargets(pid)
        yield (addedOids, removedOids)

      // Recalcula the target of a calibration observation
      def recalculateCalibrationTarget(
        pid: Program.Id,
        oid: Observation.Id,
      )(using Transaction[F]): F[Unit] = {
        for {
          o    <- session.execute(Statements.selectCalibrationTimeAndConf)(oid).map(_.headOption)
          // Find the original target
          otgs <- o.map(_._1).map { oid =>
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
                                NonEmptyList.one(oid),
                                Some(NonEmptyList.one(ct)),
                                NonEmptyList.fromList(otgs))
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
