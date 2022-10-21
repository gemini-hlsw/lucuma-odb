// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Sync
import cats.syntax.functor.*
import cats.syntax.monoid.*
import cats.syntax.traverse.*
import edu.gemini.grackle.Result
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.math.Wavelength
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.odb.graphql.input.GmosNorthLongSlitInput
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GmosCodecs.*
import natchez.Trace
import skunk.*
import skunk.codec.text.text
import skunk.implicits.*


trait GmosLongSlitService[F[_]] {

  def insertNorth(
    observationId: Observation.Id,
    input:         GmosNorthLongSlitInput.Create,
    xa:            Transaction[F]
  ): F[Unit]

}

object GmosLongSlitService {

  def fromSession[F[_]: Sync: Trace](
    session: Session[F]
  ): GmosLongSlitService[F] =

    new GmosLongSlitService[F] {

      override def insertNorth(
        observationId: Observation.Id,
        input:         GmosNorthLongSlitInput.Create,
        xa:            Transaction[F]
      ): F[Unit] = {
        val af = Statements.insertGmosNorthLongSlit(observationId, input)
        session.prepare(af.fragment.command).use { pq =>
          pq.execute(af.argument).void
        }
      }

    }

  object Statements {

    val InsertGmosNorthLongSlit: Fragment[
      Observation.Id          ~
      GmosNorthGrating        ~
      Option[GmosNorthFilter] ~
      GmosNorthFpu            ~
      Wavelength              ~
      Option[GmosXBinning]    ~
      Option[GmosYBinning]    ~
      Option[GmosAmpReadMode] ~
      Option[GmosAmpGain]     ~
      Option[GmosRoi]         ~
      Option[String]          ~
      Option[String]          ~
      GmosNorthGrating        ~
      Option[GmosNorthFilter] ~
      GmosNorthFpu            ~
      Wavelength
    ] =
      sql"""
        INSERT INTO t_gmos_north_long_slit (
          c_observation_id,
          c_grating,
          c_filter,
          c_fpu,
          c_central_wavelength,
          c_xbin,
          c_ybin,
          c_amp_read_mode,
          c_amp_gain,
          c_roi,
          c_wavelength_dithers,
          c_spatial_offsets,
          c_initial_grating,
          c_initial_filter,
          c_initial_fpu,
          c_initial_central_wavelength
        )
        SELECT
          $observation_id,
          $gmos_north_grating,
          ${gmos_north_filter.opt},
          $gmos_north_fpu,
          $wavelength_pm,
          ${gmos_x_binning.opt},
          ${gmos_y_binning.opt},
          ${gmos_amp_read_mode.opt},
          ${gmos_amp_gain.opt},
          ${gmos_roi.opt},
          ${text.opt},
          ${text.opt},
          $gmos_north_grating,
          ${gmos_north_filter.opt},
          $gmos_north_fpu,
          $wavelength_pm
       """

    def insertGmosNorthLongSlit(
      observationId: Observation.Id,
      input:         GmosNorthLongSlitInput.Create
    ): AppliedFragment =
      InsertGmosNorthLongSlit.apply(
        observationId                   ~
          input.grating                 ~
          input.filter                  ~
          input.fpu                     ~
          input.centralWavelength       ~
          input.explicitXBin            ~
          input.explicitYBin            ~
          input.explicitAmpReadMode     ~
          input.explicitAmpGain         ~
          input.explicitRoi             ~
          input.formattedλDithers       ~
          input.formattedSpatialOffsets ~
          input.grating                 ~
          input.filter                  ~
          input.fpu                     ~
          input.centralWavelength
      )

  }

}
