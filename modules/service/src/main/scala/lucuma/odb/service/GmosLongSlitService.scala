// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Sync
import cats.syntax.functor.*
import cats.syntax.monoid.*
import cats.syntax.traverse.*
import edu.gemini.grackle.Result
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.math.Wavelength
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.odb.graphql.input.GmosNorthLongSlitInput
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GmosCodecs.*
import natchez.Trace
import skunk.*
import skunk.implicits.*


trait GmosLongSlitService[F[_]] {

  def createGmosNorthLongSlit(
    observationId: Observation.Id,
    input:         GmosNorthLongSlitInput.Create
  ): F[Result[Unit]]

}

/*

  c_observation_id     d_observation_id NOT NULL PRIMARY KEY REFERENCES t_observation(c_observation_id),

  c_grating            d_tag           NOT NULL             REFERENCES t_gmos_north_disperser(c_tag),
  c_filter             d_tag           NULL DEFAULT NULL    REFERENCES t_gmos_north_filter(c_tag),
  c_fpu                d_tag           NOT NULL             REFERENCES t_gmos_north_fpu(c_tag),
  c_central_wavelength d_wavelength_pm NULL DEFAULT NULL,

  c_xbin               d_tag           NULL DEFAULT NULL   REFERENCES t_gmos_binning(c_tag),
  c_ybin               d_tag           NULL DEFAULT NULL   REFERENCES t_gmos_binning(c_tag),
  c_amp_read_mode      d_tag           NULL DEFAULT NULL   REFERENCES t_gmos_amp_read_mode(c_tag),
  c_amp_gain           d_tag           NULL DEFAULT NULL   REFERENCES t_gmos_amp_gain(c_tag),
  c_roi                d_tag           NULL DEFAULT NULL   REFERENCES t_gmos_roi(c_tag),

  -- stuff wavelength dithers and offsets into a string until grackle supports array columns?
  c_wavelength_dithers text            NULL DEFAULT NULL,
  c_spatial_offsets    text            NULL DEFAULT NULL,

  -- hold on to the initial grating, filter, fpu and central wavelength regardless of subsequent changes
  c_initial_grating            d_tag           NOT NULL             REFERENCES t_gmos_north_disperser(c_tag),
  c_initial_filter             d_tag           NULL DEFAULT NULL    REFERENCES t_gmos_north_filter(c_tag),
  c_initial_fpu                d_tag           NOT NULL             REFERENCES t_gmos_north_fpu(c_tag),
  c_initial_central_wavelength d_wavelength_pm NULL DEFAULT NULL,

  CONSTRAINT wavelength_dither_format CHECK (c_wavelength_dithers ~ '^-?\d+(?:,-?\d+)*$'),
  CONSTRAINT offset_format            CHECK (c_spatial_offsets ~ '^\(-?\d+,-?\d+\)(?:,\(-?\d+,-?\d+\))*$')
*/

object GmosLongSlitService {

  def fromSessionAndUser[F[_]: Sync: Trace](
    session: Session[F],
    user:    User
  ): GmosLongSlitService[F] =

    new GmosLongSlitService[F] {

      override def createGmosNorthLongSlit(
        observationId: Observation.Id,
        input:         GmosNorthLongSlitInput.Create
      ): F[Result[Unit]] = {
        val af = Statements.insertGmosNorthLongSlit(observationId, input)
        session.prepare(af.fragment.command).use { pq =>
          pq.execute(af.argument).as(Result.unit)
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
        observationId              ~
          input.grating            ~
          input.filter             ~
          input.fpu                ~
          input.centralWavelength  ~
          input.grating            ~
          input.filter             ~
          input.fpu                ~
          input.centralWavelength
      )

  }

}
