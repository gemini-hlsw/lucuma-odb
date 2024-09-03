// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.Nested
import cats.data.NonEmptyList
import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.Site
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.Parallax
import lucuma.core.math.ProperMotion
import lucuma.core.math.RadialVelocity
import lucuma.core.math.RightAscension
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Target
import lucuma.core.util.Timestamp
import lucuma.odb.data.Existence
import lucuma.odb.data.GroupTree
import lucuma.odb.data.ObservingModeType
import lucuma.odb.graphql.input.CreateGroupInput
import lucuma.odb.graphql.input.GroupPropertiesInput
import lucuma.odb.sequence.gmos.longslit.Config
import lucuma.odb.service.ConfigForCalibrations.*
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

      private def allObservations(pid: Program.Id, selection: ObservationSelection)(using Transaction[F]): F[(List[(Observation.Id, Config.GmosNorth | Config.GmosSouth)])] =
        services.generatorParamsService.selectAll(pid, selection = selection)
          .map(_.toList.map(_.map(_.map(_.observingMode))).collect {
            case (oid, Right(mode: Config.GmosNorth)) => (oid, mode)
            case (oid, Right(mode: Config.GmosSouth)) => (oid, mode)
          })

      private def uniqueConfiguration(all: List[(Observation.Id, Config.GmosNorth | Config.GmosSouth)]): List[(Observation.Id, ConfigForCalibrations)] = {
        val gnLSDiff =
          all.collect {
            case (oid, mode: Config.GmosNorth) => (oid, mode)
          }.distinctBy((_, m) =>
              (m.grating,
              m.filter,
              m.fpu,
              m.centralWavelength,
              m.xBin,
              m.yBin
          )).map {
            case (oid, c @ Config.GmosNorth(g, of, f, w, _, _, _, _, _, _, _, _, _)) =>
              (oid, GmosNConfigs(g, of, f, w, c.xBin, c.yBin))
          }

        val gsLSDiff =
          all.collect {
            case (oid, mode: Config.GmosSouth) => (oid, mode)
          }.distinctBy((_, m) =>
              (m.grating,
              m.filter,
              m.fpu,
              m.centralWavelength,
              m.xBin,
              m.yBin
          )).map {
            case (oid, c @ Config.GmosSouth(g, of, f, w, _, _, _, _, _, _, _, _, _)) =>
              (oid, GmosSConfigs(g, of, f, w, c.xBin, c.yBin))
          }

        gnLSDiff ::: gsLSDiff
      }

      private def toCalibConfig(all: List[(Observation.Id, Config.GmosNorth | Config.GmosSouth)]): List[(Observation.Id, ConfigForCalibrations)] =
        all.collect {
          case (oid, c @ Config.GmosNorth(g, of, f, w, _, _, _, _, _, _, _, _, _)) =>
            (oid, GmosNConfigs(g, of, f, w, c.xBin, c.yBin))
          case (oid, c @ Config.GmosSouth(g, of, f, w, _, _, _, _, _, _, _, _, _)) =>
            (oid, GmosSConfigs(g, of, f, w, c.xBin, c.yBin))
        }

      private def calibObservation(calibRole: CalibrationRole, site: Site, pid: Program.Id, gid: Group.Id, configs: List[ConfigForCalibrations], tid: Target.Id)(using Transaction[F]): F[List[Observation.Id]] =
        calibRole match {
          case CalibrationRole.SpectroPhotometric =>
            site match {
              case Site.GN  => gnLSSpecPhotoObs(pid, gid, configs.collect { case c: GmosNConfigs => c }, tid)
              case Site.GS  => gsLSSpecPhotoObs(pid, gid, configs.collect { case c: GmosSConfigs => c }, tid)
            }
          case CalibrationRole.Twilight =>
            site match {
              case Site.GN  => gnLSTwilightObs(pid, gid, configs.collect { case c: GmosNConfigs => c }, tid)
              case Site.GS  => gsLSTwilightObs(pid, gid, configs.collect { case c: GmosSConfigs => c }, tid)
            }
          case _ => List.empty.pure[F]
        }

      private def gmosCalibrations(
        pid: Program.Id,
        gid: Group.Id,
        configs: List[ConfigForCalibrations],
        gnCalc: CalibrationIdealTargets,
        gsCalc: CalibrationIdealTargets
      )(using Transaction[F]): F[List[(CalibrationRole, Observation.Id)]] = {
        def newCalibs(site: Site, ct: CalibrationRole, idealTarget: CalibrationIdealTargets): Option[F[List[(CalibrationRole, Observation.Id)]]] =
          idealTarget.bestTarget(ct).map(tgtid =>
            // We don't want to create a target if there are no pending configurations
            if (configs.nonEmpty) {
              (for {
                cta <- Nested(targetService.cloneTargetInto(tgtid, pid)).map(_._2).value
                o   <- cta.traverse(calibObservation(ct, site, pid, gid, configs, _))
              } yield o).orError.map(_.map((ct, _)))
            } else {
              List.empty[(CalibrationRole, Observation.Id)].pure[F]
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
        configs: List[ConfigForCalibrations],
        gnTgt: CalibrationIdealTargets,
        gsTgt: CalibrationIdealTargets
      )(using Transaction[F]): F[List[Observation.Id]] = {
        if (configs.isEmpty) {
          List.empty.pure[F]
        } else
          for {
            cg   <- calibrationsGroup(pid, configs.size)
            oids <- cg.map(g =>
                      gmosCalibrations(pid, g, configs, gnTgt, gsTgt).flatTap { oids =>
                        oids.groupBy(_._1).toList.traverse { case (role, oids) =>
                          setCalibRoleAndGroup(oids.map(_._2), role).whenA(oids.nonEmpty)
                        }
                      }
                    ).getOrElse(List.empty.pure[F])
          } yield oids.map(_._2)
      }

      private def removeUnnecessaryCalibrations(
        scienceConfigs: List[ConfigForCalibrations],
        calibrations: List[(Observation.Id, ConfigForCalibrations)]
      )(using Transaction[F]): F[List[Observation.Id]] = {
        val os = calibrations.collect { case (oid, c) if (!scienceConfigs.exists(_ === c)) => oid }
        val oids = NonEmptyList.fromList(os)
        oids
          .map(oids => observationService.deleteCalibrationObservations(oids).as(oids.toList))
          .getOrElse(List.empty.pure[F])
      }

      override def calibrationTargets(roles: List[CalibrationRole], referenceInstant: Instant)
        : F[List[(Target.Id, String, CalibrationRole, Coordinates)]] =
          session.execute(Statements.selectCalibrationTargets(roles))(roles)
            .map(targetCoordinates(referenceInstant))

      def recalculateCalibrations(pid: Program.Id, referenceInstant: Instant)(using Transaction[F]): F[(List[Observation.Id], List[Observation.Id])] = {

        for {
          tgts          <- calibrationTargets(CalibrationTypes, referenceInstant)
          gsTgt         = CalibrationIdealTargets(Site.GS, referenceInstant, tgts)
          gnTgt         = CalibrationIdealTargets(Site.GN, referenceInstant, tgts)
          uniqueSci     <- allObservations(pid, ObservationSelection.Science).map(uniqueConfiguration)
          allCalibs     <- allObservations(pid, ObservationSelection.Calibration)
          uniqueCalibs  = uniqueConfiguration(allCalibs)
          calibs        = toCalibConfig(allCalibs)
          configs       = uniqueSci.map(_._2).diff(uniqueCalibs.map(_._2))
          addedOids     <- generateGMOSLSCalibrations(pid, configs, gnTgt, gsTgt)
          removedOids   <- removeUnnecessaryCalibrations(uniqueSci.map(_._2), calibs)
        } yield (addedOids, List.empty)
      }

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

  }

}

