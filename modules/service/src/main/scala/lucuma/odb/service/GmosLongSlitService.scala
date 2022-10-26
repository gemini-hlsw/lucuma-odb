// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Applicative
import cats.data.NonEmptyList
import cats.effect.Sync
import cats.syntax.foldable.*
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
import lucuma.odb.data.Nullable
import lucuma.odb.data.Nullable.Absent
import lucuma.odb.data.Nullable.NonNull
import lucuma.odb.graphql.input.GmosNorthLongSlitInput
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GmosCodecs.*
import skunk.*
import skunk.codec.text.text
import skunk.implicits.*


trait GmosLongSlitService[F[_]] {

  def insertNorth(
    input: GmosNorthLongSlitInput.Create
  )(
    which: List[Observation.Id],
    xa:    Transaction[F]
  ): F[Unit]

  def deleteNorth(
    which: List[Observation.Id],
    xa:    Transaction[F]
  ): F[Unit]

  def updateNorth(
    SET:   GmosNorthLongSlitInput.Edit
  )(
    which: List[Observation.Id],
    xa:    Transaction[F]
  ): F[Unit]

}

object GmosLongSlitService {

  def fromSession[F[_]: Sync](
    session: Session[F]
  ): GmosLongSlitService[F] =

    new GmosLongSlitService[F] {

      override def insertNorth(
        input: GmosNorthLongSlitInput.Create,
      )(
        which: List[Observation.Id],
        xa:    Transaction[F]
      ): F[Unit] =
        which.traverse { oid =>
          val af = Statements.insertGmosNorthLongSlit(oid, input)
          session.prepare(af.fragment.command).use { pq =>
            pq.execute(af.argument).void
          }
        }.void

      override def deleteNorth(
        which: List[Observation.Id],
        xa:    Transaction[F]
      ): F[Unit] =
        Statements
          .deleteGmosNorthLongSlit(which)
          .fold(Applicative[F].unit) { af =>
            session.prepare(af.fragment.command).use { pq =>
              pq.execute(af.argument).void
            }
          }

      override def updateNorth(
        SET:   GmosNorthLongSlitInput.Edit
      )(
        which: List[Observation.Id],
        xa:    Transaction[F]
      ): F[Unit] =
        Statements
          .updateGmosNorthLongSlit(SET, which)
          .fold(Applicative[F].unit) { af =>
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

    private def observationIdIn(
      oids: NonEmptyList[Observation.Id]
    ): AppliedFragment =
      void"c_observation_id IN (" |+|
        oids.map(sql"$observation_id").intercalate(void", ") |+|
      void")"

    def deleteGmosNorthLongSlit(
      which: List[Observation.Id]
    ): Option[AppliedFragment] =
      NonEmptyList.fromList(which).map { oids =>
        void"DELETE FROM ONLY t_gmos_north_long_slit " |+|
          void"WHERE " |+| observationIdIn(oids)
      }

    def gmosNorthUpdates(
      input: GmosNorthLongSlitInput.Edit
    ): Option[NonEmptyList[AppliedFragment]] = {

      val upGrating     = sql"c_grating            = $gmos_north_grating"
      val upFilter      = sql"c_filter             = ${gmos_north_filter.opt}"
      val upFpu         = sql"c_fpu                = $gmos_north_fpu"
      val upCentralλ    = sql"c_central_wavelength = $wavelength_pm"
      val upXBin        = sql"c_xbin               = ${gmos_x_binning.opt}"
      val upYBin        = sql"c_ybin               = ${gmos_y_binning.opt}"
      val upAmpReadMode = sql"c_amp_read_mode      = ${gmos_amp_read_mode.opt}"
      val upAmpGain     = sql"c_amp_gain           = ${gmos_amp_gain.opt}"
      val upRoi         = sql"c_roi                = ${gmos_roi.opt}"
      val upλDithers    = sql"c_wavelength_dithers = ${text.opt}"
      val upOffsets     = sql"c_spatial_offsets    = ${text.opt}"

      val ups: List[AppliedFragment] =
        List(
          input.grating.map(upGrating),
          input.filter.toOptionOption.map(upFilter),
          input.fpu.map(upFpu),
          input.centralWavelength.map(upCentralλ),
          input.explicitXBin.toOptionOption.map(upXBin),
          input.explicitYBin.toOptionOption.map(upYBin),
          input.explicitAmpReadMode.toOptionOption.map(upAmpReadMode),
          input.explicitAmpGain.toOptionOption.map(upAmpGain),
          input.explicitRoi.toOptionOption.map(upRoi),
          input.formattedλDithers.toOptionOption.map(upλDithers),
          input.formattedSpatialOffsets.toOptionOption.map(upOffsets)
        ).flatten

      NonEmptyList.fromList(ups)
    }

    def updateGmosNorthLongSlit(
      SET:   GmosNorthLongSlitInput.Edit,
      which: List[Observation.Id]
    ): Option[AppliedFragment] =

      for {
        us   <- gmosNorthUpdates(SET)
        oids <- NonEmptyList.fromList(which)
      } yield
        void"UPDATE t_gmos_north_long_slit " |+|
          void"SET " |+| us.intercalate(void", ") |+| void" " |+|
          void"WHERE " |+| observationIdIn(oids)


  }

}
