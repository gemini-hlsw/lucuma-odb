// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.FlatMap
import cats.syntax.all.*
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.math.Wavelength
import lucuma.core.model.Program
import lucuma.odb.service.Services.Syntax.*
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GmosCodecs.*
import org.typelevel.log4cats.Logger
import skunk.Query
import skunk.syntax.all.*

trait CalibrationsService[F[_]] {
  def recalculateCalibrations(
    pid: Program.Id
  )(using L: Logger[F]): F[Unit]

}

object CalibrationsService {
  def instantiate[F[_]: FlatMap](using Services[F]): CalibrationsService[F] =
    new CalibrationsService[F] {
      def recalculateCalibrations(pid: Program.Id)(using L: Logger[F]): F[Unit] =
        for {
          gnls   <- session.execute(Statements.SelectGmosNorthLongSlitConfigurations)(pid)
          gsls   <- session.execute(Statements.SelectGmosSouthLongSlitConfigurations)(pid)
          groups <- groupService.selectGroups(pid)
          _      <- Logger[F].info(groups.toString)
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

