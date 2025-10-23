// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Applicative
import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.all.*
import grackle.Result
import grackle.ResultT
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
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.odb.data.ExposureTimeModeRole
import lucuma.odb.graphql.input.GmosLongSlitInput
import lucuma.odb.sequence.gmos.longslit.AcquisitionConfig
import lucuma.odb.sequence.gmos.longslit.Config.Common
import lucuma.odb.sequence.gmos.longslit.Config.GmosNorth
import lucuma.odb.sequence.gmos.longslit.Config.GmosSouth
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GmosCodecs.*
import skunk.*
import skunk.codec.text.text
import skunk.implicits.*

import Services.Syntax.*

trait GmosLongSlitService[F[_]] {

  def selectNorth(
    which: List[Observation.Id]
  ): F[Map[Observation.Id, GmosNorth]]

  def selectSouth(
    which: List[Observation.Id]
  ): F[Map[Observation.Id, GmosSouth]]

  def insertNorth(
    input:  GmosLongSlitInput.Create.North,
    reqEtm: Option[ExposureTimeMode],
    which:  List[Observation.Id]
  )(using Transaction[F]): F[Result[Unit]]

  def insertSouth(
    input:  GmosLongSlitInput.Create.South,
    reqEtm: Option[ExposureTimeMode],
    which:  List[Observation.Id]
  )(using Transaction[F]): F[Result[Unit]]

  def deleteNorth(which: List[Observation.Id])(using Transaction[F]): F[Unit]

  def deleteSouth(which: List[Observation.Id])(using Transaction[F]): F[Unit]

  def updateNorth(
    SET:   GmosLongSlitInput.Edit.North,
    which: List[Observation.Id]
  )(using Transaction[F]): F[Unit]

  def updateSouth(
    SET: GmosLongSlitInput.Edit.South,
    which: List[Observation.Id]
  )(using Transaction[F]): F[Unit]

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

  def instantiate[F[_]: Concurrent](using Services[F]): GmosLongSlitService[F] =

    new GmosLongSlitService[F] {

      val common: Decoder[Common] =
        (wavelength_pm          *:   // centralWavelength
         exposure_time_mode     *:   // science exposure time mode
         gmos_binning           *:   // defaultXBin
         gmos_binning.opt       *:   // explicitXBin
         gmos_binning           *:   // defaultYBin
         gmos_binning.opt       *:   // explicitYBin
         gmos_amp_read_mode.opt *:   // explicitAmpReadMode
         gmos_amp_gain.opt      *:   // explicitAmpGain
         gmos_roi.opt           *:   // explicitRoi
         text.opt               *:   // explicitWavelengthDithers
         text.opt                    // explicitOffsets
        ).emap: (w, exp, defaultX, x, defaultY, y, arm, ag, roi, owd, oso) =>
          for
            wavelengthDithers <- owd.traverse(wd => GmosLongSlitInput.WavelengthDithersFormat.getOption(wd).toRight(s"Could not parse '$wd' as a wavelength dithers list."))
            offsets           <- oso.traverse(sd => GmosLongSlitInput.SpatialOffsetsFormat.getOption(sd).toRight(s"Could not parse '$sd' as as offsets list."))
          yield Common(
            w,
            exp,
            GmosXBinning(defaultX),
            x.map(GmosXBinning(_)),
            GmosYBinning(defaultY),
            y.map(GmosYBinning(_)),
            arm,
            ag,
            roi,
            wavelengthDithers,
            offsets
          )

      val north_acquisition: Decoder[AcquisitionConfig.GmosNorth] =
        (exposure_time_mode     *: // acquisition exposure time mode
         gmos_north_filter      *: // default acquisition filter
         gmos_north_filter.opt     // explicit acquisition filter (if any)
        ).to[AcquisitionConfig.GmosNorth]

      val south_acquisition: Decoder[AcquisitionConfig.GmosSouth] =
        (exposure_time_mode     *: // acquisition exposure time mode
         gmos_south_filter      *: // default acquisition filter
         gmos_south_filter.opt     // explicit acquisition filter (if any)
        ).to[AcquisitionConfig.GmosSouth]

      val north: Decoder[GmosNorth] =
        (gmos_north_grating     *:
         gmos_north_filter.opt  *: // science filter (if any)
         gmos_north_fpu         *:
         common                 *:
         north_acquisition
        ).to[GmosNorth]

      val south: Decoder[GmosSouth] =
        (gmos_south_grating     *:
         gmos_south_filter.opt  *: // science filter (if any)
         gmos_south_fpu         *:
         common                 *:
         south_acquisition
        ).to[GmosSouth]

      private def select[A](
        which:   List[Observation.Id],
        f:       NonEmptyList[Observation.Id] => AppliedFragment,
        decoder: Decoder[A]
      ): F[List[(Observation.Id, A)]] =
        NonEmptyList
          .fromList(which)
          .fold(Applicative[F].pure(List.empty)): oids =>
            val af = f(oids)
            session.prepareR(af.fragment.query(observation_id *: decoder)).use: pq =>
              pq.stream(af.argument, chunkSize = 1024).compile.toList

      override def selectNorth(
        which: List[Observation.Id]
      ): F[Map[Observation.Id, GmosNorth]] =
        select(which, Statements.selectGmosNorthLongSlit, north).map(_.toMap)

      override def selectSouth(
        which: List[Observation.Id]
      ): F[Map[Observation.Id, GmosSouth]] =
        select(which, Statements.selectGmosSouthLongSlit, south).map(_.toMap)

      private def insertExposureTimeModes(
        name:   String,
        acq:    Option[ExposureTimeMode],
        sci:    Option[ExposureTimeMode],
        req:    Option[ExposureTimeMode],
        which:  List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        exposureTimeModeService.insertForSingleScienceEtm(name, acq, sci, req, which)

      override def insertNorth(
        input: GmosLongSlitInput.Create.North,
        req:   Option[ExposureTimeMode],
        which: List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        (for
          _ <- ResultT(insertExposureTimeModes("GMOS North Long Slit", input.acquisition.flatMap(_.exposureTimeMode), input.common.exposureTimeMode, req, which))
          _ <- ResultT.liftF(which.traverse { oid => session.exec(Statements.insertGmosNorthLongSlit(oid, input)) }.void)
        yield ()).value

      override def insertSouth(
        input: GmosLongSlitInput.Create.South,
        req:   Option[ExposureTimeMode],
        which: List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        (for
          _ <- ResultT(insertExposureTimeModes("GMOS South Long Slit", input.acquisition.flatMap(_.exposureTimeMode), input.common.exposureTimeMode, req, which))
          _ <- ResultT.liftF(which.traverse { oid => session.exec(Statements.insertGmosSouthLongSlit(oid, input)) }.void)
        yield ()).value

      override def deleteNorth(which: List[Observation.Id])(using Transaction[F]): F[Unit] =
        Statements.deleteGmosNorthLongSlit(which).fold(Applicative[F].unit)(session.exec)

      override def deleteSouth(which: List[Observation.Id])(using Transaction[F]): F[Unit] =
        Statements.deleteGmosSouthLongSlit(which).fold(Applicative[F].unit)(session.exec)

      private def updateExposureTimeModes(
        acq:   Option[ExposureTimeMode],
        sci:   Option[ExposureTimeMode],
        which: List[Observation.Id]
      )(using Transaction[F]): F[Unit] =

        def update(etm: Option[ExposureTimeMode], role: ExposureTimeModeRole): F[Unit] =
          NonEmptyList.fromList(which).traverse_ : nel =>
            etm.fold(().pure[F]): e =>
              services.exposureTimeModeService.updateMany(nel, role, e)

        for
          _ <- update(acq, ExposureTimeModeRole.Acquisition)
          _ <- update(sci, ExposureTimeModeRole.Science)
        yield ()

      override def updateNorth(
        SET:   GmosLongSlitInput.Edit.North,
        which: List[Observation.Id]
      )(using Transaction[F]): F[Unit] =
        for
          _ <- updateExposureTimeModes(SET.acquisition.flatMap(_.exposureTimeMode), SET.common.exposureTimeMode, which)
          _ <- Statements.updateGmosNorthLongSlit(SET, which).fold(Applicative[F].unit)(session.exec)
        yield ()

      override def updateSouth(
        SET: GmosLongSlitInput.Edit.South,
        which: List[Observation.Id]
      )(using Transaction[F]): F[Unit] =
        for
          _ <- updateExposureTimeModes(SET.acquisition.flatMap(_.exposureTimeMode), SET.common.exposureTimeMode, which)
          _ <- Statements.updateGmosSouthLongSlit(SET, which).fold(Applicative[F].unit)(session.exec)
        yield ()

      def cloneNorth(
        originalId: Observation.Id,
        newId: Observation.Id,
      ): F[Unit] =
        session.exec(Statements.cloneGmosNorthLongSlit(originalId, newId))

      def cloneSouth(
        originalId: Observation.Id,
        newId: Observation.Id,
      ): F[Unit] =
        session.exec(Statements.cloneGmosSouthLongSlit(originalId, newId))

    }

  object Statements {

    private def selectGmosLongSlit(
      table: String,
      observationIds: NonEmptyList[Observation.Id]
    ): AppliedFragment =
      sql"""
        SELECT
          ls.c_observation_id,
          ls.c_grating,
          ls.c_filter,
          ls.c_fpu,
          ls.c_central_wavelength,
          sci.c_exposure_time_mode,
          sci.c_signal_to_noise_at,
          sci.c_signal_to_noise,
          sci.c_exposure_time,
          sci.c_exposure_count,
          ls.c_xbin_default,
          ls.c_xbin,
          ls.c_ybin_default,
          ls.c_ybin,
          ls.c_amp_read_mode,
          ls.c_amp_gain,
          ls.c_roi,
          ls.c_wavelength_dithers,
          ls.c_offsets,
          acq.c_exposure_time_mode,
          acq.c_signal_to_noise_at,
          acq.c_signal_to_noise,
          acq.c_exposure_time,
          acq.c_exposure_count,
          ls.c_acquisition_filter_default,
          ls.c_acquisition_filter
        FROM
          #$table ls
        LEFT JOIN t_exposure_time_mode acq
           ON acq.c_observation_id = ls.c_observation_id
          AND acq.c_role = 'acquisition'
        LEFT JOIN t_exposure_time_mode sci
           ON sci.c_observation_id = ls.c_observation_id
          AND sci.c_role = 'science'
      """(Void) |+|
      void"""
        WHERE
          ls.c_observation_id IN ("""                                     |+|
            observationIds.map(sql"$observation_id").intercalate(void",") |+|
          void")"

    def selectGmosNorthLongSlit(
      observationIds: NonEmptyList[Observation.Id]
    ): AppliedFragment =
      selectGmosLongSlit("v_gmos_north_long_slit", observationIds)

    def selectGmosSouthLongSlit(
      observationIds: NonEmptyList[Observation.Id]
    ): AppliedFragment =
      selectGmosLongSlit("v_gmos_south_long_slit", observationIds)

    val InsertGmosNorthLongSlit: Fragment[(
      Observation.Id          ,
      GmosNorthGrating        ,
      Option[GmosNorthFilter] ,
      Option[GmosNorthFilter] ,
      GmosNorthFpu            ,
      Wavelength              ,
      Option[GmosXBinning]    ,
      Option[GmosYBinning]    ,
      Option[GmosAmpReadMode] ,
      Option[GmosAmpGain]     ,
      Option[GmosRoi]         ,
      Option[String]          ,
      Option[String]          ,
      GmosNorthGrating        ,
      Option[GmosNorthFilter] ,
      GmosNorthFpu            ,
      Wavelength
    )] =
      sql"""
        INSERT INTO t_gmos_north_long_slit (
          c_observation_id,
          c_program_id,
          c_grating,
          c_filter,
          c_acquisition_filter,
          c_fpu,
          c_central_wavelength,
          c_xbin,
          c_ybin,
          c_amp_read_mode,
          c_amp_gain,
          c_roi,
          c_wavelength_dithers,
          c_offsets,
          c_initial_grating,
          c_initial_filter,
          c_initial_fpu,
          c_initial_central_wavelength
        )
        SELECT
          $observation_id,
          c_program_id,
          $gmos_north_grating,
          ${gmos_north_filter.opt},
          ${gmos_north_filter.opt},
          $gmos_north_fpu,
          $wavelength_pm,
          ${gmos_binning.opt},
          ${gmos_binning.opt},
          ${gmos_amp_read_mode.opt},
          ${gmos_amp_gain.opt},
          ${gmos_roi.opt},
          ${text.opt},
          ${text.opt},
          $gmos_north_grating,
          ${gmos_north_filter.opt},
          $gmos_north_fpu,
          $wavelength_pm
        FROM t_observation
        WHERE c_observation_id = $observation_id
       """.contramap { (o, g, l, af, u, w, x, y, r, n, i, wd, so, ig, il, iu, iw) => (
         o, g, l, af, u, w, x.map(_.value), y.map(_.value), r, n, i, wd, so, ig, il, iu, iw, o
       )}

    def insertGmosNorthLongSlit(
      observationId: Observation.Id,
      input:         GmosLongSlitInput.Create.North
    ): AppliedFragment =
      InsertGmosNorthLongSlit.apply(
        observationId,
        input.grating,
        input.filter,
        input.acquisition.flatMap(_.filter.toOption),
        input.fpu,
        input.common.centralWavelength,
        input.common.explicitXBin,
        input.common.explicitYBin,
        input.common.explicitAmpReadMode,
        input.common.explicitAmpGain,
        input.common.explicitRoi,
        input.common.formattedλDithers,
        input.common.formattedOffsets,
        input.grating,
        input.filter,
        input.fpu,
        input.common.centralWavelength
      )

    val InsertGmosSouthLongSlit: Fragment[(
      Observation.Id          ,
      GmosSouthGrating        ,
      Option[GmosSouthFilter] ,
      Option[GmosSouthFilter] ,
      GmosSouthFpu            ,
      Wavelength              ,
      Option[GmosXBinning]    ,
      Option[GmosYBinning]    ,
      Option[GmosAmpReadMode] ,
      Option[GmosAmpGain]     ,
      Option[GmosRoi]         ,
      Option[String]          ,
      Option[String]          ,
      GmosSouthGrating        ,
      Option[GmosSouthFilter] ,
      GmosSouthFpu            ,
      Wavelength
    )] =
      sql"""
        INSERT INTO t_gmos_south_long_slit (
          c_observation_id,
          c_program_id,
          c_grating,
          c_filter,
          c_acquisition_filter,
          c_fpu,
          c_central_wavelength,
          c_xbin,
          c_ybin,
          c_amp_read_mode,
          c_amp_gain,
          c_roi,
          c_wavelength_dithers,
          c_offsets,
          c_initial_grating,
          c_initial_filter,
          c_initial_fpu,
          c_initial_central_wavelength
        )
        SELECT
          $observation_id,
          c_program_id,
          $gmos_south_grating,
          ${gmos_south_filter.opt},
          ${gmos_south_filter.opt},
          $gmos_south_fpu,
          $wavelength_pm,
          ${gmos_binning.opt},
          ${gmos_binning.opt},
          ${gmos_amp_read_mode.opt},
          ${gmos_amp_gain.opt},
          ${gmos_roi.opt},
          ${text.opt},
          ${text.opt},
          $gmos_south_grating,
          ${gmos_south_filter.opt},
          $gmos_south_fpu,
          $wavelength_pm
        FROM t_observation
        WHERE c_observation_id = $observation_id
       """.contramap { (o, g, l, af, u, w, x, y, r, n, i, wd, so, ig, il, iu, iw) => (
         o, g, l, af, u, w, x.map(_.value), y.map(_.value), r, n, i, wd, so, ig, il, iu, iw, o
       )}

    def insertGmosSouthLongSlit(
      observationId: Observation.Id,
      input:         GmosLongSlitInput.Create.South
    ): AppliedFragment =
      InsertGmosSouthLongSlit.apply(
        observationId,
        input.grating,
        input.filter,
        input.acquisition.flatMap(_.filter.toOption),
        input.fpu,
        input.common.centralWavelength,
        input.common.explicitXBin,
        input.common.explicitYBin,
        input.common.explicitAmpReadMode,
        input.common.explicitAmpGain,
        input.common.explicitRoi,
        input.common.formattedλDithers,
        input.common.formattedOffsets,
        input.grating,
        input.filter,
        input.fpu,
        input.common.centralWavelength
      )

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
      val upXBin        = sql"c_xbin               = ${gmos_binning.opt}"
      val upYBin        = sql"c_ybin               = ${gmos_binning.opt}"
      val upAmpReadMode = sql"c_amp_read_mode      = ${gmos_amp_read_mode.opt}"
      val upAmpGain     = sql"c_amp_gain           = ${gmos_amp_gain.opt}"
      val upRoi         = sql"c_roi                = ${gmos_roi.opt}"
      val upλDithers    = sql"c_wavelength_dithers = ${text.opt}"
      val upOffsets     = sql"c_offsets            = ${text.opt}"

      List(
        input.centralWavelength.map(upCentralλ),
        input.explicitXBin.toOptionOption.map(b => upXBin(b.map(_.value))),
        input.explicitYBin.toOptionOption.map(b => upYBin(b.map(_.value))),
        input.explicitAmpReadMode.toOptionOption.map(upAmpReadMode),
        input.explicitAmpGain.toOptionOption.map(upAmpGain),
        input.explicitRoi.toOptionOption.map(upRoi),
        input.formattedλDithers.toOptionOption.map(upλDithers),
        input.formattedOffsets.toOptionOption.map(upOffsets)
      ).flatten
    }

    def gmosNorthUpdates(
      input: GmosLongSlitInput.Edit.North
    ): Option[NonEmptyList[AppliedFragment]] = {

      val upGrating     = sql"c_grating            = $gmos_north_grating"
      val upFilter      = sql"c_filter             = ${gmos_north_filter.opt}"
      val upAcqFilter   = sql"c_acquisition_filter = ${gmos_north_filter.opt}"
      val upFpu         = sql"c_fpu                = $gmos_north_fpu"

      val ups: List[AppliedFragment] =
        List(
          input.grating.map(upGrating),
          input.filter.toOptionOption.map(upFilter),
          input.acquisition.flatMap(_.filter.toOptionOption).map(upAcqFilter),
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

      val upGrating     = sql"c_grating            = $gmos_south_grating"
      val upFilter      = sql"c_filter             = ${gmos_south_filter.opt}"
      val upAcqFilter   = sql"c_acquisition_filter = ${gmos_south_filter.opt}"
      val upFpu         = sql"c_fpu                = $gmos_south_fpu"

      val ups: List[AppliedFragment] =
        List(
          input.grating.map(upGrating),
          input.filter.toOptionOption.map(upFilter),
          input.filter.toOptionOption.map(upAcqFilter),
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

    private def cloneGmosLongSlit(
      table: String,
      originalId: Observation.Id,
      newId: Observation.Id
    ): AppliedFragment =
      sql"""
      INSERT INTO #$table (
        c_observation_id,
        c_program_id,
        c_observing_mode_type,
        c_grating,
        c_filter,
        c_acquisition_filter,
        c_fpu,
        c_central_wavelength,
        c_xbin,
        c_ybin,
        c_amp_read_mode,
        c_amp_gain,
        c_roi,
        c_wavelength_dithers,
        c_offsets,
        c_initial_grating,
        c_initial_filter,
        c_initial_fpu,
        c_initial_central_wavelength
      )
      SELECT
        $observation_id,
        (SELECT c_program_id FROM t_observation WHERE c_observation_id = $observation_id) AS c_program_id,
        c_observing_mode_type,
        c_grating,
        c_filter,
        c_acquisition_filter,
        c_fpu,
        c_central_wavelength,
        c_xbin,
        c_ybin,
        c_amp_read_mode,
        c_amp_gain,
        c_roi,
        c_wavelength_dithers,
        c_offsets,
        c_initial_grating,
        c_initial_filter,
        c_initial_fpu,
        c_initial_central_wavelength
      FROM #$table
      WHERE c_observation_id = $observation_id
      """.apply(newId, newId, originalId)

    def cloneGmosNorthLongSlit(originalId: Observation.Id, newId: Observation.Id): AppliedFragment =
      cloneGmosLongSlit("t_gmos_north_long_slit", originalId, newId)

    def cloneGmosSouthLongSlit(originalId: Observation.Id, newId: Observation.Id): AppliedFragment =
      cloneGmosLongSlit("t_gmos_south_long_slit", originalId, newId)

  }
}
