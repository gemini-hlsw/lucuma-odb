// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Applicative
import cats.data.NonEmptyList
import cats.effect.Sync
import cats.syntax.all.*
import edu.gemini.grackle.Result
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.math.Wavelength
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.odb.data.Nullable
import lucuma.odb.data.Nullable.Absent
import lucuma.odb.data.Nullable.NonNull
import lucuma.odb.graphql.input.GmosLongSlitInput
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GmosCodecs.*
import skunk.*
import skunk.codec.text.text
import skunk.implicits.*


trait GmosLongSlitService[F[_]] {

  def selectNorth(
    which: List[Observation.Id]
  ): F[Map[Observation.Id, GmosLongSlitInput.Create.North]]

  def selectSouth(
    which: List[Observation.Id]
  ): F[Map[Observation.Id, GmosLongSlitInput.Create.South]]

  def insertNorth(
    input: GmosLongSlitInput.Create.North
  )(
    which: List[Observation.Id],
    xa:    Transaction[F]
  ): F[Unit]

  def insertSouth(
    input: GmosLongSlitInput.Create.South
  )(
    which: List[Observation.Id],
    xa:    Transaction[F]
  ): F[Unit]

  def deleteNorth(
    which: List[Observation.Id],
    xa:    Transaction[F]
  ): F[Unit]

  def deleteSouth(
    which: List[Observation.Id],
    xa:    Transaction[F]
  ): F[Unit]

  def updateNorth(
    SET:   GmosLongSlitInput.Edit.North
  )(
    which: List[Observation.Id],
    xa:    Transaction[F]
  ): F[Unit]

  def updateSouth(
    SET: GmosLongSlitInput.Edit.South
  )(
    which: List[Observation.Id],
    xa:    Transaction[F]
  ): F[Unit]

  def cloneNorth(
    originalId: Observation.Id,
    newId: Observation.Id,
  ): F[Unit]

  def cloneSouth(
    originalId: Observation.Id,
    newId: Observation.Id,
  ): F[Unit] 

}

object GmosLongSlitService {

  def fromSession[F[_]: Sync](
    session: Session[F]
  ): GmosLongSlitService[F] =

    new GmosLongSlitService[F] {

      val common: Decoder[GmosLongSlitInput.Create.Common] =
        (wavelength_pm          ~
         gmos_x_binning.opt     ~
         gmos_y_binning.opt     ~
         gmos_amp_read_mode.opt ~
         gmos_amp_gain.opt      ~
         gmos_roi.opt           ~
         text.opt               ~
         text.opt
        ).emap { case (((((((w, x), y), arm), ag), roi), owd), osd) =>
          for {
            wavelengthDithers <- owd.traverse(wd => GmosLongSlitInput.WavelengthDithersFormat.getOption(wd).toRight(s"Could not parse '$wd' as a wavelength dithers list."))
            spatialDithers    <- osd.traverse(sd => GmosLongSlitInput.SpatialOffsetsFormat.getOption(sd).toRight(s"Could not parse '$sd' as a spatial offsets list."))
          } yield GmosLongSlitInput.Create.Common(w, x, y, arm, ag, roi, wavelengthDithers, spatialDithers)
        }

      val north: Decoder[GmosLongSlitInput.Create.North] =
        (gmos_north_grating     ~
         gmos_north_filter.opt  ~
         gmos_north_fpu         ~
         common
        ).gmap[GmosLongSlitInput.Create.North]

      val south: Decoder[GmosLongSlitInput.Create.South] =
        (gmos_south_grating     ~
         gmos_south_filter.opt  ~
         gmos_south_fpu         ~
         common
        ).gmap[GmosLongSlitInput.Create.South]

      private def select[A](
        which:   List[Observation.Id],
        f:       NonEmptyList[Observation.Id] => AppliedFragment,
        decoder: Decoder[A]
      ): F[Map[Observation.Id, A]] =
        NonEmptyList
          .fromList(which)
          .fold(Applicative[F].pure(Map.empty)) { oids =>
            val af = f(oids)
            session.prepareR(af.fragment.query(observation_id ~ decoder)).use { pq =>
              pq.stream(af.argument, chunkSize = 1024).compile.toList.map(_.toMap)
            }
          }

      override def selectNorth(
        which: List[Observation.Id]
      ): F[Map[Observation.Id, GmosLongSlitInput.Create.North]] =
        select(which, Statements.selectGmosNorthLongSlit, north)

      override def selectSouth(
        which: List[Observation.Id]
      ): F[Map[Observation.Id, GmosLongSlitInput.Create.South]] =
        select(which, Statements.selectGmosSouthLongSlit, south)

      private def exec(af: AppliedFragment): F[Unit] =
        session.prepareR(af.fragment.command).use { pq =>
          pq.execute(af.argument).void
        }

      override def insertNorth(
        input: GmosLongSlitInput.Create.North,
      )(
        which: List[Observation.Id],
        xa:    Transaction[F]
      ): F[Unit] =
        which.traverse { oid => exec(Statements.insertGmosNorthLongSlit(oid, input)) }.void

      override def insertSouth(
        input: GmosLongSlitInput.Create.South,
      )(
        which: List[Observation.Id],
        xa:    Transaction[F]
      ): F[Unit] =
        which.traverse { oid => exec(Statements.insertGmosSouthLongSlit(oid, input)) }.void

      override def deleteNorth(
        which: List[Observation.Id],
        xa:    Transaction[F]
      ): F[Unit] =
        Statements.deleteGmosNorthLongSlit(which).fold(Applicative[F].unit)(exec)

      override def deleteSouth(
        which: List[Observation.Id],
        xa:    Transaction[F]
      ): F[Unit] =
        Statements.deleteGmosSouthLongSlit(which).fold(Applicative[F].unit)(exec)

      override def updateNorth(
        SET:   GmosLongSlitInput.Edit.North
      )(
        which: List[Observation.Id],
        xa:    Transaction[F]
      ): F[Unit] =
        Statements.updateGmosNorthLongSlit(SET, which).fold(Applicative[F].unit)(exec)

      override def updateSouth(
        SET: GmosLongSlitInput.Edit.South
      )(
        which: List[Observation.Id],
        xa:    Transaction[F]
      ): F[Unit] =
        Statements.updateGmosSouthLongSlit(SET, which).fold(Applicative[F].unit)(exec)

      def cloneNorth(
        originalId: Observation.Id,
        newId: Observation.Id,
      ): F[Unit] =
        exec(Statements.cloneGmosNorthLongSlit(originalId, newId))

      def cloneSouth(
        originalId: Observation.Id,
        newId: Observation.Id,
      ): F[Unit] =
        exec(Statements.cloneGmosSouthLongSlit(originalId, newId))      

    }

  object Statements {

    private def selectGmosLongSlit(
      table: String,
      observationIds: NonEmptyList[Observation.Id]
    ): AppliedFragment =
      sql"""
        SELECT
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
          c_spatial_offsets
        FROM
          #$table
      """(Void) |+|
      void"""
        WHERE
          c_observation_id IN ("""                                        |+|
            observationIds.map(sql"$observation_id").intercalate(void",") |+|
          void")"

    def selectGmosNorthLongSlit(
      observationIds: NonEmptyList[Observation.Id]
    ): AppliedFragment =
      selectGmosLongSlit("t_gmos_north_long_slit", observationIds)

    def selectGmosSouthLongSlit(
      observationIds: NonEmptyList[Observation.Id]
    ): AppliedFragment =
      selectGmosLongSlit("t_gmos_south_long_slit", observationIds)

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
      input:         GmosLongSlitInput.Create.North
    ): AppliedFragment =
      InsertGmosNorthLongSlit.apply(
        observationId                          ~
          input.grating                        ~
          input.filter                         ~
          input.fpu                            ~
          input.common.centralWavelength       ~
          input.common.explicitXBin            ~
          input.common.explicitYBin            ~
          input.common.explicitAmpReadMode     ~
          input.common.explicitAmpGain         ~
          input.common.explicitRoi             ~
          input.common.formattedλDithers       ~
          input.common.formattedSpatialOffsets ~
          input.grating                        ~
          input.filter                         ~
          input.fpu                            ~
          input.common.centralWavelength
      )

    val InsertGmosSouthLongSlit: Fragment[
      Observation.Id          ~
      GmosSouthGrating        ~
      Option[GmosSouthFilter] ~
      GmosSouthFpu            ~
      Wavelength              ~
      Option[GmosXBinning]    ~
      Option[GmosYBinning]    ~
      Option[GmosAmpReadMode] ~
      Option[GmosAmpGain]     ~
      Option[GmosRoi]         ~
      Option[String]          ~
      Option[String]          ~
      GmosSouthGrating        ~
      Option[GmosSouthFilter] ~
      GmosSouthFpu            ~
      Wavelength
    ] =
      sql"""
        INSERT INTO t_gmos_south_long_slit (
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
          $gmos_south_grating,
          ${gmos_south_filter.opt},
          $gmos_south_fpu,
          $wavelength_pm,
          ${gmos_x_binning.opt},
          ${gmos_y_binning.opt},
          ${gmos_amp_read_mode.opt},
          ${gmos_amp_gain.opt},
          ${gmos_roi.opt},
          ${text.opt},
          ${text.opt},
          $gmos_south_grating,
          ${gmos_south_filter.opt},
          $gmos_south_fpu,
          $wavelength_pm
       """

    def insertGmosSouthLongSlit(
      observationId: Observation.Id,
      input:         GmosLongSlitInput.Create.South
    ): AppliedFragment =
      InsertGmosSouthLongSlit.apply(
        observationId                          ~
          input.grating                        ~
          input.filter                         ~
          input.fpu                            ~
          input.common.centralWavelength       ~
          input.common.explicitXBin            ~
          input.common.explicitYBin            ~
          input.common.explicitAmpReadMode     ~
          input.common.explicitAmpGain         ~
          input.common.explicitRoi             ~
          input.common.formattedλDithers       ~
          input.common.formattedSpatialOffsets ~
          input.grating                        ~
          input.filter                         ~
          input.fpu                            ~
          input.common.centralWavelength
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

    def deleteGmosSouthLongSlit(
      which: List[Observation.Id]
    ): Option[AppliedFragment] =
      NonEmptyList.fromList(which).map { oids =>
        void"DELETE FROM ONLY t_gmos_south_long_slit " |+|
          void"WHERE " |+| observationIdIn(oids)
      }

    def commonUpdates(
      input: GmosLongSlitInput.Edit.Common
    ): List[AppliedFragment] = {
      val upCentralλ    = sql"c_central_wavelength = $wavelength_pm"
      val upXBin        = sql"c_xbin               = ${gmos_x_binning.opt}"
      val upYBin        = sql"c_ybin               = ${gmos_y_binning.opt}"
      val upAmpReadMode = sql"c_amp_read_mode      = ${gmos_amp_read_mode.opt}"
      val upAmpGain     = sql"c_amp_gain           = ${gmos_amp_gain.opt}"
      val upRoi         = sql"c_roi                = ${gmos_roi.opt}"
      val upλDithers    = sql"c_wavelength_dithers = ${text.opt}"
      val upOffsets     = sql"c_spatial_offsets    = ${text.opt}"

      List(
        input.centralWavelength.map(upCentralλ),
        input.explicitXBin.toOptionOption.map(upXBin),
        input.explicitYBin.toOptionOption.map(upYBin),
        input.explicitAmpReadMode.toOptionOption.map(upAmpReadMode),
        input.explicitAmpGain.toOptionOption.map(upAmpGain),
        input.explicitRoi.toOptionOption.map(upRoi),
        input.formattedλDithers.toOptionOption.map(upλDithers),
        input.formattedSpatialOffsets.toOptionOption.map(upOffsets)
      ).flatten
    }

    def gmosNorthUpdates(
      input: GmosLongSlitInput.Edit.North
    ): Option[NonEmptyList[AppliedFragment]] = {

      val upGrating     = sql"c_grating = $gmos_north_grating"
      val upFilter      = sql"c_filter  = ${gmos_north_filter.opt}"
      val upFpu         = sql"c_fpu     = $gmos_north_fpu"

      val ups: List[AppliedFragment] =
        List(
          input.grating.map(upGrating),
          input.filter.toOptionOption.map(upFilter),
          input.fpu.map(upFpu),
        ).flatten ++ commonUpdates(input.common)

      NonEmptyList.fromList(ups)
    }

    def updateGmosNorthLongSlit(
      SET:   GmosLongSlitInput.Edit.North,
      which: List[Observation.Id]
    ): Option[AppliedFragment] =

      for {
        us   <- gmosNorthUpdates(SET)
        oids <- NonEmptyList.fromList(which)
      } yield
        void"UPDATE t_gmos_north_long_slit " |+|
          void"SET " |+| us.intercalate(void", ") |+| void" " |+|
          void"WHERE " |+| observationIdIn(oids)

    def gmosSouthUpdates(
      input: GmosLongSlitInput.Edit.South
    ): Option[NonEmptyList[AppliedFragment]] = {

      val upGrating     = sql"c_grating = $gmos_south_grating"
      val upFilter      = sql"c_filter  = ${gmos_south_filter.opt}"
      val upFpu         = sql"c_fpu     = $gmos_south_fpu"

      val ups: List[AppliedFragment] =
        List(
          input.grating.map(upGrating),
          input.filter.toOptionOption.map(upFilter),
          input.fpu.map(upFpu),
        ).flatten ++ commonUpdates(input.common)

      NonEmptyList.fromList(ups)
    }

    def updateGmosSouthLongSlit(
      SET:   GmosLongSlitInput.Edit.South,
      which: List[Observation.Id]
    ): Option[AppliedFragment] =

      for {
        us   <- gmosSouthUpdates(SET)
        oids <- NonEmptyList.fromList(which)
      } yield
        void"UPDATE t_gmos_south_long_slit " |+|
          void"SET " |+| us.intercalate(void", ") |+| void" " |+|
          void"WHERE " |+| observationIdIn(oids)

    def cloneGmosNorthLongSlit(originalId: Observation.Id, newId: Observation.Id): AppliedFragment =
      sql"""
      INSERT INTO t_gmos_north_long_slit (
        c_observation_id,
        c_observing_mode_type,
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
        c_observing_mode_type,
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
      FROM t_gmos_north_long_slit
      WHERE c_observation_id = $observation_id
      """.apply(newId ~ originalId)

    def cloneGmosSouthLongSlit(originalId: Observation.Id, newId: Observation.Id): AppliedFragment =
      sql"""
      INSERT INTO t_gmos_south_long_slit (
        c_observation_id,
        c_observing_mode_type,
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
        c_observing_mode_type,
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
      FROM t_gmos_south_long_slit
      WHERE c_observation_id = $observation_id
      """.apply(newId ~ originalId)

  }
}
