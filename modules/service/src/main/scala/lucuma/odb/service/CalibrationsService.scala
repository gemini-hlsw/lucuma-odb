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
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.math.Wavelength
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.odb.data.CalibrationRole
import lucuma.odb.data.Existence
import lucuma.odb.data.GroupTree
import lucuma.odb.graphql.input.CreateGroupInput
import lucuma.odb.graphql.input.CreateObservationInput
import lucuma.odb.graphql.input.GroupPropertiesInput
import lucuma.odb.graphql.input.ObservationPropertiesInput
import lucuma.odb.service.Services.Syntax.*
import lucuma.odb.util.*
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GmosCodecs.*
import lucuma.refined.*
import skunk.AppliedFragment
import skunk.Query
import skunk.Transaction
import skunk.data.Completion
import skunk.syntax.all.*

trait CalibrationsService[F[_]] {
  def recalculateCalibrations(
    pid: Program.Id
  )(using Transaction[F]): F[Unit]

}

object CalibrationsService {
  val CalibrationsGroupName: NonEmptyString = "Calibrations".refined
  private type GmosNConfigs = (GmosNorthGrating, GmosNorthFpu, Wavelength, Option[GmosXBinning], Option[GmosYBinning])
  private type GmosSConfigs = (GmosSouthGrating, GmosSouthFpu, Wavelength, Option[GmosXBinning], Option[GmosYBinning])

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

      private def calibrationObservations(pid: Program.Id,  gnls: List[GmosNConfigs], gsls: List[GmosSConfigs])(using Transaction[F]): F[Result[List[Observation.Id]]] = {
        val gmosNorthLSObservations = gnls.traverse { case (g, f, w, xb, yb) =>
          observationService.createObservation(CreateObservationInput(
            programId = pid.some,
            proposalReference = none,
            programReference = none,
            SET = ObservationPropertiesInput.Create.Default.some
          ))
        }
        val gmosSouthLSObservations = gsls.traverse { case (g, f, w, xb, yb) =>
          observationService.createObservation(CreateObservationInput(
            programId = pid.some,
            proposalReference = none,
            programReference = none,
            SET = ObservationPropertiesInput.Create.Default.some
          ))
        }
        (for {
          o1 <- gmosNorthLSObservations
          o2 <- gmosSouthLSObservations
        } yield (o1 ::: o2)).map(_.sequence)
      }

      private def setObservationCalibRole(oids: List[Observation.Id], calibrationRole: CalibrationRole)(using Transaction[F]): F[Completion] = {
        val update = void"UPDATE t_observation " |+|
          sql"SET c_calibration_role = $calibration_role "(calibrationRole) |+|
          void"WHERE c_observation_id IN (" |+|
            oids.map(sql"$observation_id").intercalate(void", ") |+| void")"
        session.executeCommand(update)
      }

      private def generateCalibrations(pid: Program.Id, gnls: List[GmosNConfigs], gsls: List[GmosSConfigs])(using Transaction[F]): F[Unit] = {
        for {
          cg  <- calibrationsGroup(pid, gnls.size + gsls.size)
          _   <- calibrationObservations(pid, gnls, gsls).flatMap {
                   case Result.Success(ids) if ids.nonEmpty =>
                     setObservationCalibRole(ids, CalibrationRole.SpectroPhotometric).void
                   case _ => Applicative[F].unit
                 }
        } yield ()
      }

      def recalculateCalibrations(pid: Program.Id)(using Transaction[F]): F[Unit] =
        for {
          gnls <- session.execute(Statements.selectGmosNorthLongSlitConfigurations(false))(pid)
          gsls <- session.execute(Statements.selectGmosSouthLongSlitConfigurations(false))(pid)
          _    <- generateCalibrations(pid, gnls, gsls).whenA(gnls.nonEmpty || gsls.nonEmpty)
        } yield ()
    }

  object Statements {

    def selectGmosNorthLongSlitConfigurations(includeCalibs: Boolean): Query[Program.Id, GmosNConfigs] = {
      val noCalibs = sql"c_calibration_role is null"
      val calibs   = sql"c_calibration_role is not null"
      val selector = if (includeCalibs) calibs else noCalibs

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
         WHERE c_program_id=$program_id and $selector
      """.query(gmos_north_grating *: gmos_north_fpu *: wavelength_pm *: gmos_x_binning.opt *: gmos_y_binning.opt)
  }

    def selectGmosSouthLongSlitConfigurations(includeCalibs: Boolean): Query[Program.Id, GmosSConfigs] = {
      val noCalibs = sql"c_calibration_role is null"
      val calibs   = sql"c_calibration_role is not null"
      val selector = if (includeCalibs) calibs else noCalibs

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
  }

}

