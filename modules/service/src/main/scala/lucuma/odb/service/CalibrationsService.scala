// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Applicative
import cats.MonadThrow
import cats.data.Nested
import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosRoi
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
import lucuma.odb.data.CalibrationRole
import lucuma.odb.data.Existence
import lucuma.odb.data.GroupTree
import lucuma.odb.data.Nullable
import lucuma.odb.data.PosAngleConstraintMode
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
import lucuma.odb.service.Services.Syntax.*
import lucuma.odb.util.*
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GmosCodecs.*
import lucuma.refined.*
import skunk.AppliedFragment
import skunk.Query
import skunk.Transaction
import skunk.codec.numeric.int8
import skunk.syntax.all.*

import java.time.Instant

trait CalibrationsService[F[_]] {
  def recalculateCalibrations(
    pid: Program.Id,
    referenceInstant: Instant
  )(using Transaction[F]): F[Unit]
}

object CalibrationsService {
  type GmosNConfigs = (GmosNorthGrating, GmosNorthFpu, Wavelength, Option[GmosXBinning], Option[GmosYBinning])
  type GmosSConfigs = (GmosSouthGrating, GmosSouthFpu, Wavelength, Option[GmosXBinning], Option[GmosYBinning])
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
                        )
                      ),
                    system = true
                  ).map(_.toOption)
              }
            case _ => none.pure[F]
          }
        } else none.pure[F]

      private def createCalibrationObservation(pid: Program.Id, gid: Group.Id, tid: Target.Id, create: ObservingModeInput.Create)(using Transaction[F]): F[Observation.Id] =
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
                      group = gid.some,
                      posAngleConstraint = PosAngleConstraintInput(
                        mode = PosAngleConstraintMode.AverageParallactic.some, none
                      ).some,
                      observingMode = create.some,
                      scienceRequirements =
                        ScienceRequirementsInput(
                          mode = ScienceMode.Spectroscopy.some,
                          spectroscopy = SpectroscopyScienceRequirementsInput.Default.copy(
                            signalToNoise = Nullable.NonNull(SignalToNoise.unsafeFromBigDecimalExact(100.0))
                        ).some
                      ).some
                    ).some
            )
          ).orError

      private def calibrationObservations(pid: Program.Id, gid: Group.Id, gnls: List[GmosNConfigs], gsls: List[GmosSConfigs], gnTgt: Option[Target.Id], gsTgt: Option[Target.Id])(using Transaction[F]): F[List[Observation.Id]] = {
        def gmosNorthLSObservations(gnls: List[GmosNConfigs], tid: Target.Id): F[List[Observation.Id]] =
          gnls.traverse { case (g, f, w, xb, yb) =>
            val conf =
              GmosLongSlitInput.Create.North(
                grating = g,
                filter = none,
                fpu = f,
                common = GmosLongSlitInput.Create.Common(
                  centralWavelength = w,
                  explicitXBin = xb,
                  explicitYBin = yb,
                  explicitAmpReadMode = none,
                  explicitAmpGain = none,
                  explicitRoi = GmosRoi.CentralSpectrum.some,
                  explicitλDithers = none,
                  explicitSpatialOffsets = none
                )
              )

            createCalibrationObservation(pid, gid, tid, ObservingModeInput.Create(conf.some, none))
          }

        def gmosSouthLSObservations(gsls: List[GmosSConfigs], tid: Target.Id): F[List[Observation.Id]] =
          gsls.traverse { case (g, f, w, xb, yb) =>
            val conf =
              GmosLongSlitInput.Create.South(
                grating = g,
                filter = none,
                fpu = f,
                common = GmosLongSlitInput.Create.Common(
                  centralWavelength = w,
                  explicitXBin = xb,
                  explicitYBin = yb,
                  explicitAmpReadMode = none,
                  explicitAmpGain = none,
                  explicitRoi = GmosRoi.CentralSpectrum.some,
                  explicitλDithers = none,
                  explicitSpatialOffsets = none
                )
              )
            createCalibrationObservation(pid, gid, tid, ObservingModeInput.Create(none, conf.some))
          }

        val gno: Option[F[List[Observation.Id]]] = gnTgt.map(tgtid =>
          (session.execute(Statements.selectGmosNorthLongSlitConfigurations(true))(pid))
            .flatMap { current =>
              val pendingConfigs = gnls.diff(current)
              // We don't want to create a target if there are no pending configurations
              if (pendingConfigs.nonEmpty) {
                (for {
                  ct      <- Nested(targetService.cloneTargetInto(tgtid, pid)).map(_._2).value
                  o       <- ct.traverse(gmosNorthLSObservations(pendingConfigs, _))
                } yield o).orError
              } else {
                List.empty[Observation.Id].pure[F]
              }
            })

        val gso: Option[F[List[Observation.Id]]] = gsTgt.map(tgtid =>
          (session.execute(Statements.selectGmosSouthLongSlitConfigurations(true))(pid))
            .flatMap { current =>
              val pendingConfigs = gsls.diff(current)
              // We don't want to create a target if there are no pending configurations
              if (pendingConfigs.nonEmpty) {
                (for {
                  ct      <- Nested(targetService.cloneTargetInto(tgtid, pid)).map(_._2).value
                  o       <- ct.traverse(gmosSouthLSObservations(pendingConfigs, _))
                } yield o).orError
              } else {
                List.empty[Observation.Id].pure[F]
              }
            })

        (gno, gso).mapN((_, _).mapN(_ ::: _)).getOrElse(Nil.pure[F])
      }

      // Set the calibration role of the observations in bulk
      private def setCalibRoleAndGroup(oids: List[Observation.Id], calibrationRole: CalibrationRole)(using Transaction[F]): F[Unit] =
        session.executeCommand(Statements.setCalibRole(oids, calibrationRole)).void

      private def generateCalibrations(pid: Program.Id, gnls: List[GmosNConfigs], gsls: List[GmosSConfigs], gnTgt: Option[Target.Id], gsTgt: Option[Target.Id])(using Transaction[F]): F[Unit] = {

        for {
          cg  <- calibrationsGroup(pid, gnls.size + gsls.size)
          _   <- cg.map(g =>
                   calibrationObservations(pid, g, gnls, gsls, gnTgt, gsTgt).flatMap { oids =>
                     setCalibRoleAndGroup(oids, CalibrationRole.SpectroPhotometric).whenA(oids.nonEmpty)
                   }
                 ).getOrElse(Applicative[F].unit)
        } yield ()
      }

      private def spectroPhotometricTargets(when: Instant)(rows: List[(Target.Id, RightAscension, Declination, Epoch, Option[Long], Option[Long], Option[RadialVelocity], Option[Parallax])]): List[(Target.Id, Coordinates)] =
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

      def recalculateCalibrations(pid: Program.Id, referenceInstant: Instant)(using Transaction[F]): F[Unit] = {
        val gncoords = idealLocation(Site.GN, referenceInstant)
        val gscoords = idealLocation(Site.GS, referenceInstant)

        for {
          tgts <- session.execute(Statements.selectCalibrationTargets)(CalibrationRole.SpectroPhotometric)
            .map(spectroPhotometricTargets(referenceInstant))
          gsTgt = bestTarget(gscoords, tgts)
          gnTgt = bestTarget(gncoords, tgts)
          gnls <- session.execute(Statements.selectGmosNorthLongSlitConfigurations(false))(pid)
          gsls <- session.execute(Statements.selectGmosSouthLongSlitConfigurations(false))(pid)
          _    <- generateCalibrations(pid, gnls, gsls, gnTgt, gsTgt).whenA(gnls.nonEmpty || gsls.nonEmpty)
        } yield ()
      }
    }

  object Statements {

    def selectGmosNorthLongSlitConfigurations(calibsOnly: Boolean): Query[Program.Id, GmosNConfigs] = {
      val noCalibs = sql"t_observation.c_calibration_role is     null"
      val calibs   = sql"t_observation.c_calibration_role is not null"
      val selector = if (calibsOnly) calibs else noCalibs

      sql"""
         SELECT DISTINCT ON (
             c_grating,
             c_fpu,
             c_central_wavelength,
             c_xbin,
             c_ybin
           )
           c_grating,
           c_fpu,
           c_central_wavelength,
           c_xbin,
           c_ybin
         FROM t_gmos_north_long_slit
         INNER JOIN t_observation
         ON t_gmos_north_long_slit.c_observation_id = t_observation.c_observation_id
         WHERE t_observation.c_program_id=$program_id and $selector
      """.query(gmos_north_grating *: gmos_north_fpu *: wavelength_pm *: gmos_x_binning.opt *: gmos_y_binning.opt)
    }

    def selectGmosSouthLongSlitConfigurations(calibsOnly: Boolean): Query[Program.Id, GmosSConfigs] = {
      val noCalibs = sql"c_calibration_role is     null"
      val calibs   = sql"c_calibration_role is not null"
      val selector = if (calibsOnly) calibs else noCalibs

      sql"""
         SELECT DISTINCT ON (
             c_grating,
             c_fpu,
             c_central_wavelength,
             c_xbin,
             c_ybin
           )
           c_grating,
           c_fpu,
           c_central_wavelength,
           c_xbin,
           c_ybin
         FROM t_gmos_south_long_slit
         INNER JOIN t_observation
         ON t_gmos_south_long_slit.c_observation_id = t_observation.c_observation_id
         WHERE c_program_id=$program_id and $selector
      """.query(gmos_south_grating *: gmos_south_fpu *: wavelength_pm *: gmos_x_binning.opt *: gmos_y_binning.opt)
    }

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
  }

}
