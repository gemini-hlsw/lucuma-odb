// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Applicative
import cats.MonadThrow
import cats.data.Nested
import cats.data.NonEmptyList
import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.ScienceMode
import lucuma.core.enums.Site
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.HourAngle
import lucuma.core.math.Parallax
import lucuma.core.math.ProperMotion
import lucuma.core.math.RadialVelocity
import lucuma.core.math.RightAscension
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.math.skycalc.ImprovedSkyCalc
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Target
import lucuma.core.util.Timestamp
import lucuma.odb.data.Existence
import lucuma.odb.data.GroupTree
import lucuma.odb.data.Nullable
import lucuma.odb.data.ObservingModeType
import lucuma.odb.data.PosAngleConstraintMode
import lucuma.odb.graphql.input.ConstraintSetInput
import lucuma.odb.graphql.input.CreateGroupInput
import lucuma.odb.graphql.input.CreateObservationInput
import lucuma.odb.graphql.input.GmosLongSlitInput
import lucuma.odb.graphql.input.GroupPropertiesInput
import lucuma.odb.graphql.input.ObservationPropertiesInput
import lucuma.odb.graphql.input.ObservingModeInput
import lucuma.odb.graphql.input.PosAngleConstraintInput
import lucuma.odb.graphql.input.ScienceRequirementsInput
import lucuma.odb.graphql.input.SpectroscopyScienceRequirementsInput
import lucuma.odb.graphql.input.TargetEnvironmentInput
import lucuma.odb.sequence.gmos.longslit.Config
import lucuma.odb.service.Services.Syntax.*
import lucuma.odb.util.*
import lucuma.odb.util.Codecs.*
import lucuma.refined.*
import skunk.AppliedFragment
import skunk.Command
import skunk.Query
import skunk.Transaction
import skunk.codec.numeric.int8
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

  def recalculateCalibrationTarget(
    pid: Program.Id,
    oid: Observation.Id,
  )(using Transaction[F]): F[Unit]
}

object CalibrationsService {
  type GmosNConfigs = (GmosNorthGrating, Option[GmosNorthFilter], GmosNorthFpu, Wavelength, GmosXBinning, GmosYBinning)
  type GmosSConfigs = (GmosSouthGrating, Option[GmosSouthFilter], GmosSouthFpu, Wavelength, GmosXBinning, GmosYBinning)
  val CalibrationsGroupName: NonEmptyString = "Calibrations".refined

  extension[F[_], A](r: F[Result[A]])
    def orError(using F: MonadThrow[F]): F[A]  =
      r.flatMap {
        case Result.Success(a)       => a.pure[F]
        case Result.Warning(_, a)    => a.pure[F]
        case Result.Failure(a)       => F.raiseError(new RuntimeException(a.map(_.message).toList.mkString(", ")))
        case Result.InternalError(a) => F.raiseError(a)
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

      private def uniqueConfiguration(pid: Program.Id, selection: ObservationSelection)(using Transaction[F]): F[(List[(Observation.Id, GmosNConfigs)], List[(Observation.Id, GmosSConfigs)])] = {
        val all: F[List[(Observation.Id, Config.GmosNorth) | (Observation.Id, Config.GmosSouth)]] = services.generatorParamsService.selectAll(pid, selection = selection)
          .map(_.toList.map(_.map(_.map(_.observingMode))).collect {
            case (oid, Right(mode: Config.GmosNorth)) => (oid, mode)
            case (oid, Right(mode: Config.GmosSouth)) => (oid, mode)
          })

        val gnLSDiff = all
          .map(_.collect {
              case (oid, mode: Config.GmosNorth) => (oid, mode)
            }.distinctBy((_, m) =>
               (m.grating,
                m.filter,
                m.fpu,
                m.centralWavelength,
                m.xBin,
                m.yBin
            ))).map { _.map {
              case (oid, c @ Config.GmosNorth(g, of, f, w, _, _, _, _, _, _, _, _, _)) =>
                (oid, (g, of, f, w, c.xBin, c.yBin))
            }}

        val gsLSDiff = all
          .map(_.collect {
              case (oid, mode: Config.GmosSouth) => (oid, mode)
            }.distinctBy((_, m) =>
               (m.grating,
                m.filter,
                m.fpu,
                m.centralWavelength,
                m.xBin,
                m.yBin
            ))).map { _.map {
              case (oid, c @ Config.GmosSouth(g, of, f, w, _, _, _, _, _, _, _, _, _)) =>
                (oid, (g, of, f, w, c.xBin, c.yBin))
            }}

        (gnLSDiff, gsLSDiff).tupled
      }

      private def createCalibrationObservation(pid: Program.Id, gid: Group.Id, tid: Target.Id, cw: Wavelength, create: ObservingModeInput.Create)(using Transaction[F]): F[Observation.Id] =

          observationService.createObservation(
            CreateObservationInput(
              programId = pid.some,
              proposalReference = none,
              programReference = none,
              SET = ObservationPropertiesInput.Create.Default.copy(
                      targetEnvironment = TargetEnvironmentInput.Create(
                        none,
                        List(tid).some
                      ).some,
                      constraintSet = ConstraintSetInput.Calibration.some,
                      group = gid.some,
                      posAngleConstraint = PosAngleConstraintInput(
                        mode = PosAngleConstraintMode.AverageParallactic.some, none
                      ).some,
                      observingMode = create.some,
                      scienceRequirements =
                        ScienceRequirementsInput(
                          mode = ScienceMode.Spectroscopy.some,
                          spectroscopy = SpectroscopyScienceRequirementsInput.Default.copy(
                            signalToNoise = Nullable.NonNull(SignalToNoise.unsafeFromBigDecimalExact(100.0)),
                            signalToNoiseAt = Nullable.NonNull(cw),
                        ).some
                      ).some
                    ).some
            )
          ).orError

      private def calibrationObservations(
        pid: Program.Id,
        gid: Group.Id,
        gnls: List[GmosNConfigs],
        gsls: List[GmosSConfigs],
        gnTgt: Option[Target.Id],
        gsTgt: Option[Target.Id]
      )(using Transaction[F]): F[List[Observation.Id]] = {

        def gmosNorthLSObservations(gnls: List[GmosNConfigs], tid: Target.Id) =
          gnls.traverse { case (g, of, f, w, xb, yb) =>
            val conf =
              GmosLongSlitInput.Create.North(
                grating = g,
                filter = of,
                fpu = f,
                common = GmosLongSlitInput.Create.Common(
                  centralWavelength = w,
                  explicitXBin = xb.some,
                  explicitYBin = yb.some,
                  explicitAmpReadMode = none,
                  explicitAmpGain = none,
                  explicitRoi = GmosRoi.CentralSpectrum.some,
                  explicitλDithers = none,
                  explicitSpatialOffsets = none
                ))

            createCalibrationObservation(pid, gid, tid, w, ObservingModeInput.Create(conf.some, none))
          }

        def gmosSouthLSObservations(gsls: List[GmosSConfigs], tid: Target.Id) =
          gsls.traverse { case (g, of, f, w, xb, yb) =>
            val conf =
              GmosLongSlitInput.Create.South(
                grating = g,
                filter = of,
                fpu = f,
                common = GmosLongSlitInput.Create.Common(
                  centralWavelength = w,
                  explicitXBin = xb.some,
                  explicitYBin = yb.some,
                  explicitAmpReadMode = none,
                  explicitAmpGain = none,
                  explicitRoi = GmosRoi.CentralSpectrum.some,
                  explicitλDithers = none,
                  explicitSpatialOffsets = none
                ))

            createCalibrationObservation(pid, gid, tid, w, ObservingModeInput.Create(none, conf.some))
          }

        val gno: Option[F[List[Observation.Id]]] = gnTgt.map(tgtid =>
            // We don't want to create a target if there are no pending configurations
            if (gnls.nonEmpty) {
              (for {
                ct      <- Nested(targetService.cloneTargetInto(tgtid, pid)).map(_._2).value
                o       <- ct.traverse(gmosNorthLSObservations(gnls, _))
              } yield o).orError
            } else {
              List.empty[Observation.Id].pure[F]
            })

        val gso: Option[F[List[Observation.Id]]] = gsTgt.map(tgtid =>
            // We don't want to create a target if there are no pending configurations
            if (gsls.nonEmpty) {
              (for {
                ct      <- Nested(targetService.cloneTargetInto(tgtid, pid)).map(_._2).value
                o       <- ct.traverse(gmosSouthLSObservations(gsls, _))
              } yield o).orError
            } else {
              List.empty[Observation.Id].pure[F]
            })

        (gno, gso).mapN((_, _).mapN(_ ::: _)).getOrElse(Nil.pure[F])
      }

      // Set the calibration role of the observations in bulk
      private def setCalibRoleAndGroup(oids: List[Observation.Id], calibrationRole: CalibrationRole)(using Transaction[F]): F[Unit] =
        session.executeCommand(Statements.setCalibRole(oids, calibrationRole)).void

      private def generateCalibrations(pid: Program.Id, gnls: List[GmosNConfigs], gsls: List[GmosSConfigs], gnTgt: Option[Target.Id], gsTgt: Option[Target.Id])(using Transaction[F]): F[List[Observation.Id]] = {
        if (gnls.isEmpty && gsls.isEmpty) {
          List.empty.pure[F]
        } else
          for {
            cg   <- calibrationsGroup(pid, gnls.size + gsls.size)
            oids <- cg.map(g =>
                      calibrationObservations(pid, g, gnls, gsls, gnTgt, gsTgt).flatTap { oids =>
                        setCalibRoleAndGroup(oids, CalibrationRole.SpectroPhotometric).whenA(oids.nonEmpty)
                      }
                    ).getOrElse(List.empty.pure[F])
          } yield oids
      }

      private def removeUnnecessaryCalibrations(
        calibsNLS: List[(Observation.Id, GmosNConfigs)],
        gnls: List[(Observation.Id, GmosNConfigs)],
        calibsSLS: List[(Observation.Id, GmosSConfigs)],
        gsls: List[(Observation.Id, GmosSConfigs)]
      )(using Transaction[F]): F[List[Observation.Id]] = {
        val o1 = calibsNLS.foldLeft(List.empty[Observation.Id]) { case (l, (oid, c)) =>

          if (gnls.exists(_._2 === c)) l else oid :: l
        }
        val o2 = calibsSLS.foldLeft(List.empty[Observation.Id]) { case (l, (oid, c)) =>

          if (gsls.exists(_._2 === c)) l else oid :: l
        }
        val oids = NonEmptyList.fromList(o1 ::: o2)
        oids
          .map(oids => observationService.deleteCalibrationObservations(oids).as(oids.toList))
          .getOrElse(List.empty.pure[F])
      }

      private def spectroPhotometricTargets(when: Instant)(
        rows: List[(Target.Id, RightAscension, Declination, Epoch, Option[Long], Option[Long], Option[RadialVelocity], Option[Parallax])]
      ): List[(Target.Id, Coordinates)] =
        rows.map { case (tid, ra, dec, epoch, pmra, pmdec, rv, parallax) =>
          (tid,
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
          case (tid, Some(st)) => (tid, st)
        }

      private def bestTarget(ref: Coordinates, tgts: List[(Target.Id, Coordinates)]): Option[Target.Id] =
        tgts.minimumByOption(_._2.angularDistance(ref))(Angle.SignedAngleOrder).map(_._1)

      private def idealLocation(site: Site, referenceInstant: Instant): Coordinates = {
        val lst = ImprovedSkyCalc(site.place).getLst(referenceInstant)
        val (h, m, s, n) = (lst.getHour, lst.getMinute, lst.getSecond, lst.getNano)
        val ra = RightAscension(HourAngle.fromHMS(h - 1, m, s, 0, n / 1000))
        val dec = site.place.latitude
        Coordinates(ra, dec)
      }

      def recalculateCalibrations(pid: Program.Id, referenceInstant: Instant)(using Transaction[F]): F[(List[Observation.Id], List[Observation.Id])] = {
        val gncoords = idealLocation(Site.GN, referenceInstant)
        val gscoords = idealLocation(Site.GS, referenceInstant)

        for {
          tgts <- session.execute(Statements.selectCalibrationTargets)(CalibrationRole.SpectroPhotometric)
            .map(spectroPhotometricTargets(referenceInstant))
          gsTgt = bestTarget(gscoords, tgts)
          gnTgt = bestTarget(gncoords, tgts)
          (scienceGmosNLS, scienceGmosSLS) <- uniqueConfiguration(pid, ObservationSelection.Science)
          (calibGmosNLS, calibGmosSLS)     <- uniqueConfiguration(pid, ObservationSelection.Calibration)
          gnls  = scienceGmosNLS.map(_._2).diff(calibGmosNLS.map(_._2))
          gsls  = scienceGmosSLS.map(_._2).diff(calibGmosSLS.map(_._2))
          addedOids                        <- generateCalibrations(pid, gnls, gsls, gnTgt, gsTgt)
          removedOids                      <- removeUnnecessaryCalibrations(calibGmosNLS, scienceGmosNLS, calibGmosSLS, scienceGmosSLS)
        } yield (addedOids, removedOids)
      }

      // Recalcula the target of a calibration observation
      def recalculateCalibrationTarget(
        pid: Program.Id,
        oid: Observation.Id,
      )(using Transaction[F]): F[Unit] = {
        Applicative[F].unit
        for {
          o <- session.execute(Statements.selectCalibrationTimeAndConf)(oid).map(_.headOption)
          // Find the original target
          otgs <- o.map(_._1).map { oid =>
                    asterismService.getAsterism(pid, oid).map(_.map(_._1))
                  }.getOrElse(List.empty.pure[F])
          // Select a new target
          tgts <- o match {
                  case Some(oid, Some(ot), Some(ObservingModeType.GmosNorthLongSlit)) =>
                    val gncoords = idealLocation(Site.GN, ot.toInstant)
                    session
                      .execute(Statements.selectCalibrationTargets)(CalibrationRole.SpectroPhotometric)
                      .map(spectroPhotometricTargets(ot.toInstant).map(bestTarget(gncoords, _)))
                  case Some(oid, Some(ot), Some(ObservingModeType.GmosSouthLongSlit)) =>
                    val gscoords = idealLocation(Site.GS, ot.toInstant)
                    session
                      .execute(Statements.selectCalibrationTargets)(CalibrationRole.SpectroPhotometric)
                      .map(spectroPhotometricTargets(ot.toInstant).map(bestTarget(gscoords, _)))
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

    def selectCalibrationTargets: Query[CalibrationRole, (Target.Id, RightAscension, Declination, Epoch, Option[Long], Option[Long], Option[RadialVelocity], Option[Parallax])] =
      sql"""SELECT
              c_target_id,
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
            WHERE t_program.c_calibration_role=$calibration_role
              AND t_program.c_existence='present'
          """.query(target_id *: right_ascension *: declination *: epoch *: int8.opt *: int8.opt *: radial_velocity.opt *: parallax.opt)

    val selectCalibrationTimeAndConf: Query[Observation.Id, (Observation.Id, Option[Timestamp], Option[ObservingModeType])] =
      sql"""
        SELECT
            c_observation_id,
            c_observation_time,
            c_observing_mode_type
          FROM t_observation
          WHERE c_observation_id = $observation_id AND c_calibration_role IS NOT NULL
          """.query(observation_id *: core_timestamp.opt *: observing_mode_type.opt)

  }

}

