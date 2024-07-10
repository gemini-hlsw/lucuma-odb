// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Applicative
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
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
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
import lucuma.odb.service.Services.Syntax.*
import lucuma.odb.util.*
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GmosCodecs.*
import lucuma.refined.*
import skunk.AppliedFragment
import skunk.Query
import skunk.Transaction
import skunk.syntax.all.*
import skunk.codec.numeric.int8
import lucuma.core.model.Target
import lucuma.core.math.RightAscension
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.Angle
import lucuma.core.math.RadialVelocity
import lucuma.core.math.Parallax
import lucuma.core.model.SiderealTracking
import lucuma.core.math.Coordinates
import lucuma.core.math.ProperMotion
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

      private def calibrationObservations(pid: Program.Id, gid: Group.Id, gnls: List[GmosNConfigs], gsls: List[GmosSConfigs])(using Transaction[F]): F[Result[List[Observation.Id]]] = {
        def gmosNorthLSObservations(gnls: List[GmosNConfigs]) = gnls.traverse { case (g, f, w, xb, yb) =>
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

          observationService.createObservation(
            CreateObservationInput(
              programId = pid.some,
              proposalReference = none,
              programReference = none,
              SET = ObservationPropertiesInput.Create.Default.copy(
                      group = gid.some,
                      posAngleConstraint = PosAngleConstraintInput(
                        mode = PosAngleConstraintMode.AverageParallactic.some, none
                      ).some,
                      observingMode = ObservingModeInput.Create(conf.some, none).some,
                      scienceRequirements =
                        ScienceRequirementsInput(
                          mode = ScienceMode.Spectroscopy.some,
                          spectroscopy = SpectroscopyScienceRequirementsInput.Default.copy(
                            signalToNoise = Nullable.NonNull(SignalToNoise.unsafeFromBigDecimalExact(100.0))
                        ).some
                      ).some
                    ).some
            )
          )
        }
        def gmosSouthLSObservations(gsls: List[GmosSConfigs]) = gsls.traverse { case (g, f, w, xb, yb) =>
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

          observationService.createObservation(
            CreateObservationInput(
              programId = pid.some,
              proposalReference = none,
              programReference = none,
              SET = ObservationPropertiesInput.Create.Default.copy(
                      group = gid.some,
                      posAngleConstraint = PosAngleConstraintInput(
                        mode = PosAngleConstraintMode.AverageParallactic.some, none
                      ).some,
                      observingMode = ObservingModeInput.Create(none, conf.some).some,
                      scienceRequirements =
                        ScienceRequirementsInput(
                          mode = ScienceMode.Spectroscopy.some,
                          spectroscopy = SpectroscopyScienceRequirementsInput.Default.copy(
                            signalToNoise = Nullable.NonNull(SignalToNoise.unsafeFromBigDecimalExact(100.0))
                        ).some
                      ).some
                    ).some
            )
          )
        }

        (for {
          currGmosNLS <- session.execute(Statements.selectGmosNorthLongSlitConfigurations(true))(pid)
          currGmosSLS <- session.execute(Statements.selectGmosSouthLongSlitConfigurations(true))(pid)
          o1          <- gmosNorthLSObservations(gnls.diff(currGmosNLS))
          o2          <- gmosSouthLSObservations(gsls.diff(currGmosSLS))
        } yield (o1 ::: o2)).map(_.sequence)
      }

      // Set the calibration role of the observations in bulk
      private def setCalibRoleAndGroup(oids: List[Observation.Id], calibrationRole: CalibrationRole)(using Transaction[F]): F[Unit] =
        session.executeCommand(Statements.setCalibRole(oids, calibrationRole)).void

      private def generateCalibrations(pid: Program.Id, gnls: List[GmosNConfigs], gsls: List[GmosSConfigs])(using Transaction[F]): F[Unit] = {
        for {
          cg  <- calibrationsGroup(pid, gnls.size + gsls.size)
          _   <- cg.map(g =>
                   calibrationObservations(pid, g, gnls, gsls).flatMap {
                     case Result.Success(ids) if ids.nonEmpty =>
                       setCalibRoleAndGroup(ids, CalibrationRole.SpectroPhotometric).void
                     case _                                   =>
                       Applicative[F].unit
                   }
                 ).getOrElse(Applicative[F].unit)
        } yield ()
      }

      private def spectroPhotometricTargets(rows: List[(Target.Id, RightAscension, Declination, Epoch, Option[Long], Option[Long], Option[RadialVelocity], Option[Parallax])]): List[(Target.Id, SiderealTracking)] =
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
            )
          )
        }

      def recalculateCalibrations(pid: Program.Id, referenceInstant: Instant)(using Transaction[F]): F[Unit] =
        for {
          tgts <- session.execute(Statements.selectCalibrationTargets)(CalibrationRole.SpectroPhotometric)
          gnls <- session.execute(Statements.selectGmosNorthLongSlitConfigurations(false))(pid)
          gsls <- session.execute(Statements.selectGmosSouthLongSlitConfigurations(false))(pid)
          _    <- {pprint.pprintln(spectroPhotometricTargets(tgts).map(_.fmap(_.at(referenceInstant))));generateCalibrations(pid, gnls, gsls).whenA(gnls.nonEmpty || gsls.nonEmpty)}
        } yield ()
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

