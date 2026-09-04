// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Applicative
import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import grackle.Result
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosIfuAcquisitionRoi
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosNorthIfuFpu
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosSouthIfuFpu
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.GmosIfuAnalysis
import lucuma.core.model.Observation
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.syntax.timespan.*
import lucuma.odb.data.ExposureTimeModeRole
import lucuma.odb.data.Nullable
import lucuma.odb.format.telescopeConfigs.*
import lucuma.odb.graphql.input.GmosIfuInput
import lucuma.odb.graphql.input.GmosLongSlitInput
import lucuma.odb.sequence.gmos.ifu.AcquisitionConfig
import lucuma.odb.sequence.gmos.ifu.Config.GmosNorth
import lucuma.odb.sequence.gmos.ifu.Config.GmosSouth
import lucuma.odb.sequence.gmos.spectroscopy.Config.Common
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GmosCodecs.*
import lucuma.refined.*
import skunk.*
import skunk.codec.text.text
import skunk.implicits.*

import Services.Syntax.*

trait GmosIfuService[F[_]] {

  def selectNorth(which: List[Observation.Id]): F[Map[Observation.Id, GmosNorth]]

  def selectSouth(which: List[Observation.Id]): F[Map[Observation.Id, GmosSouth]]

  def insertNorth(
    input: GmosIfuInput.Create.North,
    req:   Option[ExposureTimeMode],
    which: List[Observation.Id]
  )(using Transaction[F]): F[Result[Unit]]

  def insertSouth(
    input: GmosIfuInput.Create.South,
    req:   Option[ExposureTimeMode],
    which: List[Observation.Id]
  )(using Transaction[F]): F[Result[Unit]]

  def deleteNorth(which: List[Observation.Id])(using Transaction[F]): F[Unit]

  def deleteSouth(which: List[Observation.Id])(using Transaction[F]): F[Unit]

  def updateNorth(
    SET:   GmosIfuInput.Edit.North,
    which: List[Observation.Id]
  )(using Transaction[F]): F[Result[Unit]]

  def updateSouth(
    SET:   GmosIfuInput.Edit.South,
    which: List[Observation.Id]
  )(using Transaction[F]): F[Result[Unit]]

  def cloneNorth(originalId: Observation.Id, newId: Observation.Id): F[Unit]

  def cloneSouth(originalId: Observation.Id, newId: Observation.Id): F[Unit]

}

object GmosIfuService {

  private val DefaultAcquisitionCount: PosInt = 10.refined

  def defaultAcquisitionExposureTimeMode(at: Wavelength): ExposureTimeMode =
    ExposureTimeMode.TimeAndCountMode(30.secondTimeSpan, DefaultAcquisitionCount, at)

  /** The table stores the analysis as two nullable columns, at most one set. */
  private def analysisColumns(a: Option[GmosIfuAnalysis]): (Option[Angle], Option[Angle]) =
    a match
      case Some(GmosIfuAnalysis.Sum(radius))    => (radius.some, none)
      case Some(GmosIfuAnalysis.Single(offset)) => (none, offset.some)
      case None                                 => (none, none)

  def instantiate[F[_]: Concurrent](using Services[F]): GmosIfuService[F] =

    new GmosIfuService[F] {

      val north_acquisition: Decoder[AcquisitionConfig.GmosNorth] =
        (exposure_time_mode          *:
         gmos_north_filter          *:
         gmos_north_filter.opt      *:
         gmos_ifu_acquisition_roi     *:
         gmos_ifu_acquisition_roi.opt
        ).to[AcquisitionConfig.GmosNorth]

      val south_acquisition: Decoder[AcquisitionConfig.GmosSouth] =
        (exposure_time_mode          *:
         gmos_south_filter          *:
         gmos_south_filter.opt      *:
         gmos_ifu_acquisition_roi     *:
         gmos_ifu_acquisition_roi.opt
        ).to[AcquisitionConfig.GmosSouth]

      val common: Decoder[Common] =
        (wavelength_pm          *:
         exposure_time_mode     *:
         gmos_binning           *:
         gmos_binning.opt       *:
         gmos_binning           *:
         gmos_binning.opt       *:
         gmos_amp_read_mode.opt *:
         gmos_amp_gain.opt      *:
         gmos_roi.opt           *:
         text.opt
        ).emap: (w, exp, defaultX, x, defaultY, y, arm, ag, roi, owd) =>
          for
            wavelengthDithers <- owd.traverse(wd => GmosLongSlitInput.WavelengthDithersFormat.getOption(wd).toRight(s"Could not parse '$wd' as a wavelength dithers list."))
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
            wavelengthDithers
          )

      /** The effective positions, explicit or default, as the view resolves them. */
      val telescope_configs: Decoder[NonEmptyList[TelescopeConfig]] =
        text.emap: json =>
          ToSkyFormat.getOption(json).toRight(s"Could not parse '$json' as telescope configs.")

      /** At most one column is set; neither means the default sampling applies. */
      val ifu_analysis: Decoder[Option[GmosIfuAnalysis]] =
        (angle_µas.opt *: angle_µas.opt).emap:
          case (Some(_), Some(_)) => "A GMOS IFU analysis cannot set both a sum radius and a single offset.".asLeft
          case (Some(r), None)    => GmosIfuAnalysis.Sum(r).some.asRight
          case (None, Some(o))    => GmosIfuAnalysis.Single(o).some.asRight
          case (None, None)       => none.asRight

      val north: Decoder[GmosNorth] =
        (gmos_north_grating    *:
         gmos_north_filter.opt *:
         gmos_north_ifu_fpu    *:
         ifu_analysis          *:
         north_acquisition     *:
         common                *:
         telescope_configs
        ).to[GmosNorth]

      val south: Decoder[GmosSouth] =
        (gmos_south_grating    *:
         gmos_south_filter.opt *:
         gmos_south_ifu_fpu    *:
         ifu_analysis          *:
         south_acquisition     *:
         common                *:
         telescope_configs
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

      override def selectNorth(which: List[Observation.Id]): F[Map[Observation.Id, GmosNorth]] =
        select(which, Statements.selectGmosNorthIfu, north).map(_.toMap)

      override def selectSouth(which: List[Observation.Id]): F[Map[Observation.Id, GmosSouth]] =
        select(which, Statements.selectGmosSouthIfu, south).map(_.toMap)

      private def acquisitionEtm(
        explicit: Option[ExposureTimeMode],
        at:       Wavelength
      ): ExposureTimeMode =
        explicit.getOrElse(defaultAcquisitionExposureTimeMode(at))

      private def insert(
        name:  String,
        input: GmosIfuInput.Create[?, ?, ?],
        acq:   ExposureTimeMode,
        req:   Option[ExposureTimeMode],
        which: List[Observation.Id],
        stmt:  Observation.Id => AppliedFragment
      )(using Transaction[F]): F[Result[Unit]] =
        exposureTimeModeService
          .insertOneWithDefaults(name, acq.some, input.common.exposureTimeMode, req, which)
          .flatMap: r =>
            r.void.traverse(_ => which.traverse(oid => session.exec(stmt(oid))).void)

      override def insertNorth(
        input: GmosIfuInput.Create.North,
        req:   Option[ExposureTimeMode],
        which: List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        insert(
          "GMOS North IFU",
          input,
          acquisitionEtm(input.acquisition.flatMap(_.exposureTimeMode), input.common.centralWavelength),
          req,
          which,
          Statements.insertGmosNorthIfu(_, input)
        )

      override def insertSouth(
        input: GmosIfuInput.Create.South,
        req:   Option[ExposureTimeMode],
        which: List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        insert(
          "GMOS South IFU",
          input,
          acquisitionEtm(input.acquisition.flatMap(_.exposureTimeMode), input.common.centralWavelength),
          req,
          which,
          Statements.insertGmosSouthIfu(_, input)
        )

      override def deleteNorth(which: List[Observation.Id])(using Transaction[F]): F[Unit] =
        Statements.deleteGmosNorthIfu(which).fold(Applicative[F].unit)(session.exec)

      override def deleteSouth(which: List[Observation.Id])(using Transaction[F]): F[Unit] =
        Statements.deleteGmosSouthIfu(which).fold(Applicative[F].unit)(session.exec)

      private def updateExposureTimeMode(
        acq:   Option[ExposureTimeMode],
        sci:   Option[ExposureTimeMode],
        which: List[Observation.Id]
      )(using Transaction[F]): F[Unit] =

        def update(etm: Option[ExposureTimeMode], role: ExposureTimeModeRole): F[Unit] =
          etm.fold(().pure[F]): e =>
            services.exposureTimeModeService.updateMany(which, role, e)

        for
          _ <- update(acq, ExposureTimeModeRole.Acquisition)
          _ <- update(sci, ExposureTimeModeRole.Science)
        yield ()

      private def update(
        acq:   Option[ExposureTimeMode],
        sci:   Option[ExposureTimeMode],
        which: List[Observation.Id],
        stmt:  Option[AppliedFragment]
      )(using Transaction[F]): F[Result[Unit]] =
        for
          _ <- updateExposureTimeMode(acq, sci, which)
          _ <- stmt.fold(Applicative[F].unit)(session.exec)
        yield Result.unit

      override def updateNorth(
        SET:   GmosIfuInput.Edit.North,
        which: List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        update(
          SET.acquisition.flatMap(_.exposureTimeMode),
          SET.common.exposureTimeMode,
          which,
          Statements.updateGmosNorthIfu(SET, which)
        )

      override def updateSouth(
        SET:   GmosIfuInput.Edit.South,
        which: List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        update(
          SET.acquisition.flatMap(_.exposureTimeMode),
          SET.common.exposureTimeMode,
          which,
          Statements.updateGmosSouthIfu(SET, which)
        )

      override def cloneNorth(originalId: Observation.Id, newId: Observation.Id): F[Unit] =
        session.exec(Statements.cloneGmosNorthIfu(originalId, newId))

      override def cloneSouth(originalId: Observation.Id, newId: Observation.Id): F[Unit] =
        session.exec(Statements.cloneGmosSouthIfu(originalId, newId))

    }

  object Statements {

    private def selectGmosIfu(
      table:          String,
      observationIds: NonEmptyList[Observation.Id]
    ): AppliedFragment =
      sql"""
        SELECT
          m.c_observation_id,
          m.c_grating,
          m.c_filter,
          m.c_fpu,
          m.c_ifu_analysis_sum_radius,
          m.c_ifu_analysis_single_offset,
          acq.c_exposure_time_mode,
          acq.c_signal_to_noise_at,
          acq.c_signal_to_noise,
          acq.c_exposure_time,
          acq.c_exposure_count,
          m.c_acquisition_filter_default,
          m.c_acquisition_filter,
          m.c_acquisition_roi_default,
          m.c_acquisition_roi,
          m.c_central_wavelength,
          sci.c_exposure_time_mode,
          sci.c_signal_to_noise_at,
          sci.c_signal_to_noise,
          sci.c_exposure_time,
          sci.c_exposure_count,
          m.c_xbin_default,
          m.c_xbin,
          m.c_ybin_default,
          m.c_ybin,
          m.c_amp_read_mode,
          m.c_amp_gain,
          m.c_roi,
          m.c_wavelength_dithers,
          m.c_telescope_configs_effective
        FROM
          #$table m
        LEFT JOIN t_exposure_time_mode acq
           ON acq.c_observation_id = m.c_observation_id
          AND acq.c_role = 'acquisition'
        LEFT JOIN t_exposure_time_mode sci
           ON sci.c_observation_id = m.c_observation_id
          AND sci.c_role = 'science'
      """(Void) |+|
      void"""
        WHERE
          m.c_observation_id IN ("""                                      |+|
            observationIds.map(sql"$observation_id").intercalate(void",") |+|
          void")"

    def selectGmosNorthIfu(observationIds: NonEmptyList[Observation.Id]): AppliedFragment =
      selectGmosIfu("v_gmos_north_ifu", observationIds)

    def selectGmosSouthIfu(observationIds: NonEmptyList[Observation.Id]): AppliedFragment =
      selectGmosIfu("v_gmos_south_ifu", observationIds)

    val InsertGmosNorthIfu: Fragment[(
      Observation.Id,
      GmosNorthGrating,
      Option[GmosNorthFilter],
      GmosNorthIfuFpu,
      Option[Angle],
      Option[Angle],
      Option[GmosNorthFilter],
      Option[GmosIfuAcquisitionRoi],
      Wavelength,
      Option[GmosXBinning],
      Option[GmosYBinning],
      Option[GmosAmpReadMode],
      Option[GmosAmpGain],
      Option[GmosRoi],
      Option[String],
      Option[String],
      GmosNorthGrating,
      Option[GmosNorthFilter],
      GmosNorthIfuFpu,
      Wavelength
    )] =
      sql"""
        INSERT INTO t_gmos_north_ifu (
          c_observation_id,
          c_program_id,
          c_grating,
          c_filter,
          c_fpu,
          c_ifu_analysis_sum_radius,
          c_ifu_analysis_single_offset,
          c_acquisition_filter,
          c_acquisition_roi,
          c_central_wavelength,
          c_xbin,
          c_ybin,
          c_amp_read_mode,
          c_amp_gain,
          c_roi,
          c_wavelength_dithers,
          c_telescope_configs,
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
          $gmos_north_ifu_fpu,
          ${angle_µas.opt},
          ${angle_µas.opt},
          ${gmos_north_filter.opt},
          ${gmos_ifu_acquisition_roi.opt},
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
          $gmos_north_ifu_fpu,
          $wavelength_pm
        FROM t_observation
        WHERE c_observation_id = $observation_id
       """.contramap { (o, g, l, u, sr, so, af, ar, w, x, y, r, n, i, wd, tc, ig, il, iu, iw) => (
         o, g, l, u, sr, so, af, ar, w, x.map(_.value), y.map(_.value), r, n, i, wd, tc, ig, il, iu, iw, o
       )}

    def insertGmosNorthIfu(
      observationId: Observation.Id,
      input:         GmosIfuInput.Create.North
    ): AppliedFragment =
      val (sumRadius, singleOffset) = analysisColumns(input.common.explicitIfuAnalysis)
      InsertGmosNorthIfu.apply(
        observationId,
        input.grating,
        input.filter,
        input.fpu,
        sumRadius,
        singleOffset,
        input.acquisition.flatMap(_.filter.toOption),
        input.acquisition.flatMap(_.roi.toOption),
        input.common.centralWavelength,
        input.common.explicitXBin,
        input.common.explicitYBin,
        input.common.explicitAmpReadMode,
        input.common.explicitAmpGain,
        input.common.explicitRoi,
        input.common.formattedLambdaDithers,
        input.common.formattedTelescopeConfigs,
        input.grating,
        input.filter,
        input.fpu,
        input.common.centralWavelength
      )

    def deleteGmosNorthIfu(which: List[Observation.Id]): Option[AppliedFragment] =
      NonEmptyList.fromList(which).map: oids =>
        void"DELETE FROM ONLY t_gmos_north_ifu " |+|
          void"WHERE " |+| observationIdIn(oids)

    def gmosNorthUpdates(
      input: GmosIfuInput.Edit.North
    ): Option[NonEmptyList[AppliedFragment]] = {

      val upGrating   = sql"c_grating            = $gmos_north_grating"
      val upFilter    = sql"c_filter             = ${gmos_north_filter.opt}"
      val upFpu       = sql"c_fpu                = $gmos_north_ifu_fpu"
      val upAcqFilter = sql"c_acquisition_filter = ${gmos_north_filter.opt}"
      val upAcqRoi    = sql"c_acquisition_roi    = ${gmos_ifu_acquisition_roi.opt}"

      val ups: List[AppliedFragment] =
        List(
          input.grating.map(upGrating),
          input.filter.toOptionOption.map(upFilter),
          input.fpu.map(upFpu),
          input.acquisition.flatMap(_.filter.toOptionOption).map(upAcqFilter),
          input.acquisition.flatMap(_.roi.toOptionOption).map(upAcqRoi)
        ).flatten ++ commonUpdates(input.common)

      NonEmptyList.fromList(ups)
    }

    def updateGmosNorthIfu(
      SET:   GmosIfuInput.Edit.North,
      which: List[Observation.Id]
    ): Option[AppliedFragment] =
      for {
        us   <- gmosNorthUpdates(SET)
        oids <- NonEmptyList.fromList(which)
      } yield
        void"UPDATE t_gmos_north_ifu " |+|
          void"SET " |+| us.intercalate(void", ") |+| void" " |+|
          void"WHERE " |+| observationIdIn(oids)

    val InsertGmosSouthIfu: Fragment[(
      Observation.Id,
      GmosSouthGrating,
      Option[GmosSouthFilter],
      GmosSouthIfuFpu,
      Option[Angle],
      Option[Angle],
      Option[GmosSouthFilter],
      Option[GmosIfuAcquisitionRoi],
      Wavelength,
      Option[GmosXBinning],
      Option[GmosYBinning],
      Option[GmosAmpReadMode],
      Option[GmosAmpGain],
      Option[GmosRoi],
      Option[String],
      Option[String],
      GmosSouthGrating,
      Option[GmosSouthFilter],
      GmosSouthIfuFpu,
      Wavelength
    )] =
      sql"""
        INSERT INTO t_gmos_south_ifu (
          c_observation_id,
          c_program_id,
          c_grating,
          c_filter,
          c_fpu,
          c_ifu_analysis_sum_radius,
          c_ifu_analysis_single_offset,
          c_acquisition_filter,
          c_acquisition_roi,
          c_central_wavelength,
          c_xbin,
          c_ybin,
          c_amp_read_mode,
          c_amp_gain,
          c_roi,
          c_wavelength_dithers,
          c_telescope_configs,
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
          $gmos_south_ifu_fpu,
          ${angle_µas.opt},
          ${angle_µas.opt},
          ${gmos_south_filter.opt},
          ${gmos_ifu_acquisition_roi.opt},
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
          $gmos_south_ifu_fpu,
          $wavelength_pm
        FROM t_observation
        WHERE c_observation_id = $observation_id
       """.contramap { (o, g, l, u, sr, so, af, ar, w, x, y, r, n, i, wd, tc, ig, il, iu, iw) => (
         o, g, l, u, sr, so, af, ar, w, x.map(_.value), y.map(_.value), r, n, i, wd, tc, ig, il, iu, iw, o
       )}

    def insertGmosSouthIfu(
      observationId: Observation.Id,
      input:         GmosIfuInput.Create.South
    ): AppliedFragment =
      val (sumRadius, singleOffset) = analysisColumns(input.common.explicitIfuAnalysis)
      InsertGmosSouthIfu.apply(
        observationId,
        input.grating,
        input.filter,
        input.fpu,
        sumRadius,
        singleOffset,
        input.acquisition.flatMap(_.filter.toOption),
        input.acquisition.flatMap(_.roi.toOption),
        input.common.centralWavelength,
        input.common.explicitXBin,
        input.common.explicitYBin,
        input.common.explicitAmpReadMode,
        input.common.explicitAmpGain,
        input.common.explicitRoi,
        input.common.formattedLambdaDithers,
        input.common.formattedTelescopeConfigs,
        input.grating,
        input.filter,
        input.fpu,
        input.common.centralWavelength
      )

    def deleteGmosSouthIfu(which: List[Observation.Id]): Option[AppliedFragment] =
      NonEmptyList.fromList(which).map: oids =>
        void"DELETE FROM ONLY t_gmos_south_ifu " |+|
          void"WHERE " |+| observationIdIn(oids)

    def gmosSouthUpdates(
      input: GmosIfuInput.Edit.South
    ): Option[NonEmptyList[AppliedFragment]] = {

      val upGrating   = sql"c_grating            = $gmos_south_grating"
      val upFilter    = sql"c_filter             = ${gmos_south_filter.opt}"
      val upFpu       = sql"c_fpu                = $gmos_south_ifu_fpu"
      val upAcqFilter = sql"c_acquisition_filter = ${gmos_south_filter.opt}"
      val upAcqRoi    = sql"c_acquisition_roi    = ${gmos_ifu_acquisition_roi.opt}"

      val ups: List[AppliedFragment] =
        List(
          input.grating.map(upGrating),
          input.filter.toOptionOption.map(upFilter),
          input.fpu.map(upFpu),
          input.acquisition.flatMap(_.filter.toOptionOption).map(upAcqFilter),
          input.acquisition.flatMap(_.roi.toOptionOption).map(upAcqRoi)
        ).flatten ++ commonUpdates(input.common)

      NonEmptyList.fromList(ups)
    }

    def updateGmosSouthIfu(
      SET:   GmosIfuInput.Edit.South,
      which: List[Observation.Id]
    ): Option[AppliedFragment] =
      for {
        us   <- gmosSouthUpdates(SET)
        oids <- NonEmptyList.fromList(which)
      } yield
        void"UPDATE t_gmos_south_ifu " |+|
          void"SET " |+| us.intercalate(void", ") |+| void" " |+|
          void"WHERE " |+| observationIdIn(oids)

    // The analysis is replaced whole: both columns are assigned so that switching
    // between the two shapes, or clearing back to the default, cannot leave a
    // stale value behind that the at-most-one constraint would then reject.
    def analysisUpdates(a: Nullable[GmosIfuAnalysis]): List[AppliedFragment] =
      val upSumRadius    = sql"c_ifu_analysis_sum_radius    = ${angle_µas.opt}"
      val upSingleOffset = sql"c_ifu_analysis_single_offset = ${angle_µas.opt}"

      a.toOptionOption.toList.flatMap: oa =>
        val (r, o) = analysisColumns(oa)
        List(upSumRadius(r), upSingleOffset(o))

    def commonUpdates(
      input: GmosIfuInput.Edit.Common
    ): List[AppliedFragment] = {
      val upCentralW     = sql"c_central_wavelength = $wavelength_pm"
      val upXBin         = sql"c_xbin               = ${gmos_binning.opt}"
      val upYBin         = sql"c_ybin               = ${gmos_binning.opt}"
      val upAmpReadMode  = sql"c_amp_read_mode      = ${gmos_amp_read_mode.opt}"
      val upAmpGain      = sql"c_amp_gain           = ${gmos_amp_gain.opt}"
      val upRoi          = sql"c_roi                = ${gmos_roi.opt}"
      val upDithers      = sql"c_wavelength_dithers = ${text.opt}"
      val upTelescopeCfg = sql"c_telescope_configs  = ${text.opt}"

      List(
        input.centralWavelength.map(upCentralW),
        input.explicitXBin.toOptionOption.map(b => upXBin(b.map(_.value))),
        input.explicitYBin.toOptionOption.map(b => upYBin(b.map(_.value))),
        input.explicitAmpReadMode.toOptionOption.map(upAmpReadMode),
        input.explicitAmpGain.toOptionOption.map(upAmpGain),
        input.explicitRoi.toOptionOption.map(upRoi),
        input.formattedLambdaDithers.toOptionOption.map(upDithers),
        input.formattedTelescopeConfigs.toOptionOption.map(upTelescopeCfg)
      ).flatten ++ analysisUpdates(input.explicitIfuAnalysis)
    }

    private def cloneGmosIfu(
      table:      String,
      originalId: Observation.Id,
      newId:      Observation.Id
    ): AppliedFragment =
      sql"""
      INSERT INTO #$table (
        c_observation_id,
        c_program_id,
        c_observing_mode_type,
        c_grating,
        c_filter,
        c_fpu,
        c_ifu_analysis_sum_radius,
        c_ifu_analysis_single_offset,
        c_acquisition_filter,
        c_acquisition_roi,
        c_central_wavelength,
        c_xbin,
        c_ybin,
        c_amp_read_mode,
        c_amp_gain,
        c_roi,
        c_wavelength_dithers,
        c_telescope_configs,
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
        c_fpu,
        c_ifu_analysis_sum_radius,
        c_ifu_analysis_single_offset,
        c_acquisition_filter,
        c_acquisition_roi,
        c_central_wavelength,
        c_xbin,
        c_ybin,
        c_amp_read_mode,
        c_amp_gain,
        c_roi,
        c_wavelength_dithers,
        c_telescope_configs,
        c_initial_grating,
        c_initial_filter,
        c_initial_fpu,
        c_initial_central_wavelength
      FROM #$table
      WHERE c_observation_id = $observation_id
      """.apply(newId, newId, originalId)

    def cloneGmosNorthIfu(originalId: Observation.Id, newId: Observation.Id): AppliedFragment =
      cloneGmosIfu("t_gmos_north_ifu", originalId, newId)

    def cloneGmosSouthIfu(originalId: Observation.Id, newId: Observation.Id): AppliedFragment =
      cloneGmosIfu("t_gmos_south_ifu", originalId, newId)

  }
}
