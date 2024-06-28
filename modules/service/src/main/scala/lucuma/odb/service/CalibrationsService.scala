// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Monad
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.math.Wavelength
import lucuma.core.model.Group
import lucuma.core.model.Program
import lucuma.odb.data.Existence
import lucuma.odb.data.GroupTree
import lucuma.odb.graphql.input.CreateGroupInput
import lucuma.odb.graphql.input.GroupPropertiesInput
import lucuma.odb.service.Services.Syntax.*
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GmosCodecs.*
import lucuma.refined.*
import skunk.Query
import skunk.Transaction
import skunk.syntax.all.*

trait CalibrationsService[F[_]] {
  def recalculateCalibrations(
    pid: Program.Id
  )(using Transaction[F]): F[Unit]

}

object CalibrationsService {
  def instantiate[F[_]: Monad](using Services[F]): CalibrationsService[F] =
    new CalibrationsService[F] {
      private val CalibrationsGroupName: NonEmptyString = "Calibrations".refined

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

      def recalculateCalibrations(pid: Program.Id)(using Transaction[F]): F[Unit] =
        for {
          gnls <- session.execute(Statements.SelectGmosNorthLongSlitConfigurations)(pid)
          gsls <- session.execute(Statements.SelectGmosSouthLongSlitConfigurations)(pid)
          _    <-  calibrationsGroup(pid, gnls.size + gsls.size)
        } yield ()
    }

  object Statements {

    val SelectGmosNorthLongSlitConfigurations: Query[Program.Id, (GmosNorthGrating, GmosNorthFpu, Wavelength, Option[GmosXBinning], Option[GmosYBinning])] =
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
         WHERE c_program_id=$program_id
      """.query(gmos_north_grating *: gmos_north_fpu *: wavelength_pm *: gmos_x_binning.opt *: gmos_y_binning.opt)

    val SelectGmosSouthLongSlitConfigurations: Query[Program.Id, (GmosSouthGrating, GmosSouthFpu, Wavelength, Option[GmosXBinning], Option[GmosYBinning])] =
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
         WHERE c_program_id=$program_id
      """.query(gmos_south_grating *: gmos_south_fpu *: wavelength_pm *: gmos_x_binning.opt *: gmos_y_binning.opt)
  }
}

