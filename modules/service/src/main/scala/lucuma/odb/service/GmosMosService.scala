// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Applicative
import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.all.*
import grackle.Result
import grackle.ResultT
import lucuma.core.enums.AttachmentType
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosCustomSlitWidth
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.math.Wavelength
import lucuma.core.model.Attachment
import lucuma.core.model.Defined
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.MaskDefinition
import lucuma.core.model.Observation
import lucuma.core.model.ToBeDefined
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.odb.data.ExposureTimeModeRole
import lucuma.core.enums.GmosMosAcquisitionType
import lucuma.odb.graphql.input.GmosLongSlitInput
import lucuma.odb.graphql.input.GmosMosInput
import lucuma.odb.sequence.gmos.SpectroscopyConfig.Common
import lucuma.odb.sequence.gmos.mos.Config.GmosNorth
import lucuma.odb.sequence.gmos.mos.Config.GmosSouth
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GmosCodecs.*
import skunk.*
import skunk.codec.text.text
import skunk.implicits.*

import Services.Syntax.*

trait GmosMosService[F[_]] {

  def selectNorth(
    which: List[Observation.Id]
  ): F[Map[Observation.Id, GmosNorth]]

  def selectSouth(
    which: List[Observation.Id]
  ): F[Map[Observation.Id, GmosSouth]]

  def insertNorth(
    input:  GmosMosInput.Create.North,
    reqEtm: Option[ExposureTimeMode],
    which:  List[Observation.Id]
  )(using Transaction[F]): F[Result[Unit]]

  def insertSouth(
    input:  GmosMosInput.Create.South,
    reqEtm: Option[ExposureTimeMode],
    which:  List[Observation.Id]
  )(using Transaction[F]): F[Result[Unit]]

  def deleteNorth(which: List[Observation.Id])(using Transaction[F]): F[Unit]

  def deleteSouth(which: List[Observation.Id])(using Transaction[F]): F[Unit]

  def updateNorth(
    SET:   GmosMosInput.Edit.North,
    which: List[Observation.Id]
  )(using Transaction[F]): F[Result[Unit]]

  def updateSouth(
    SET:   GmosMosInput.Edit.South,
    which: List[Observation.Id]
  )(using Transaction[F]): F[Result[Unit]]

  def cloneNorth(
    originalId: Observation.Id,
    newId:      Observation.Id
  ): F[Unit]

  def cloneSouth(
    originalId: Observation.Id,
    newId:      Observation.Id
  ): F[Unit]

}

object GmosMosService {

  /**
   * A nonexistent attachment, one belonging to another program, and one that is
   * not a MOS mask all arrive as the same composite foreign key violation, so
   * they get one message naming all three conditions.
   */
  val MaskAttachmentViolationMessage: String =
    "The MOS mask attachment must exist, be of type 'mos_mask', and belong to the same program as the observation."

  def instantiate[F[_]: Concurrent](using Services[F]): GmosMosService[F] =

    new GmosMosService[F] {

      val custom_mask: Decoder[GmosFpuMask.Custom] =
        (gmos_custom_slit_width *: attachment_id.opt).map: (w, oid) =>
          GmosFpuMask.Custom(oid.fold[MaskDefinition](ToBeDefined)(Defined(_)), w)

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

      val north: Decoder[GmosNorth] =
        (gmos_north_grating       *:
         gmos_north_filter.opt    *:
         custom_mask              *:
         gmos_mos_acquisition_type *:
         common
        ).to[GmosNorth]

      val south: Decoder[GmosSouth] =
        (gmos_south_grating       *:
         gmos_south_filter.opt    *:
         custom_mask              *:
         gmos_mos_acquisition_type *:
         common
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
        select(which, Statements.selectGmosNorthMos, north).map(_.toMap)

      override def selectSouth(
        which: List[Observation.Id]
      ): F[Map[Observation.Id, GmosSouth]] =
        select(which, Statements.selectGmosSouthMos, south).map(_.toMap)

      private def translateMaskViolation[A](fa: F[A]): F[Result[A]] =
        fa.map(Result.success).recover:
          case SqlState.ForeignKeyViolation(e) if e.constraintName.exists(_.contains("mask_attachment_fkey")) =>
            Result.failure(MaskAttachmentViolationMessage)

      // MOS has no acquisition sequence, so it has no acquisition exposure time mode
      private def insert(
        name:  String,
        input: GmosMosInput.Create[?, ?],
        req:   Option[ExposureTimeMode],
        which: List[Observation.Id],
        stmt:  Observation.Id => AppliedFragment
      )(using Transaction[F]): F[Result[Unit]] =
        (for
          _ <- ResultT(exposureTimeModeService.insertScienceOnlyWithDefaults(name, input.common.exposureTimeMode, req, which).map(_.void))
          _ <- ResultT(translateMaskViolation(which.traverse(oid => session.exec(stmt(oid))).void))
        yield ()).value

      override def insertNorth(
        input: GmosMosInput.Create.North,
        req:   Option[ExposureTimeMode],
        which: List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        insert("GMOS North MOS", input, req, which, Statements.insertGmosNorthMos(_, input))

      override def insertSouth(
        input: GmosMosInput.Create.South,
        req:   Option[ExposureTimeMode],
        which: List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        insert("GMOS South MOS", input, req, which, Statements.insertGmosSouthMos(_, input))

      override def deleteNorth(which: List[Observation.Id])(using Transaction[F]): F[Unit] =
        Statements.deleteGmosNorthMos(which).fold(Applicative[F].unit)(session.exec)

      override def deleteSouth(which: List[Observation.Id])(using Transaction[F]): F[Unit] =
        Statements.deleteGmosSouthMos(which).fold(Applicative[F].unit)(session.exec)

      private def updateExposureTimeMode(
        sci:   Option[ExposureTimeMode],
        which: List[Observation.Id]
      )(using Transaction[F]): F[Unit] =
        sci.fold(().pure[F]): e =>
          services.exposureTimeModeService.updateMany(which, ExposureTimeModeRole.Science, e)

      override def updateNorth(
        SET:   GmosMosInput.Edit.North,
        which: List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        for
          _ <- updateExposureTimeMode(SET.common.exposureTimeMode, which)
          r <- translateMaskViolation(Statements.updateGmosNorthMos(SET, which).fold(Applicative[F].unit)(session.exec))
        yield r

      override def updateSouth(
        SET:   GmosMosInput.Edit.South,
        which: List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        for
          _ <- updateExposureTimeMode(SET.common.exposureTimeMode, which)
          r <- translateMaskViolation(Statements.updateGmosSouthMos(SET, which).fold(Applicative[F].unit)(session.exec))
        yield r

      override def cloneNorth(
        originalId: Observation.Id,
        newId:      Observation.Id
      ): F[Unit] =
        session.exec(Statements.cloneGmosNorthMos(originalId, newId))

      override def cloneSouth(
        originalId: Observation.Id,
        newId:      Observation.Id
      ): F[Unit] =
        session.exec(Statements.cloneGmosSouthMos(originalId, newId))

    }

  object Statements {

    private def selectGmosMos(
      table:          String,
      observationIds: NonEmptyList[Observation.Id]
    ): AppliedFragment =
      sql"""
        SELECT
          m.c_observation_id,
          m.c_grating,
          m.c_filter,
          m.c_slit_width,
          m.c_mask_attachment_id,
          m.c_acquisition_type,
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
          m.c_offsets
        FROM
          #$table m
        LEFT JOIN t_exposure_time_mode sci
           ON sci.c_observation_id = m.c_observation_id
          AND sci.c_role = 'science'
      """(Void) |+|
      void"""
        WHERE
          m.c_observation_id IN ("""                                      |+|
            observationIds.map(sql"$observation_id").intercalate(void",") |+|
          void")"

    def selectGmosNorthMos(
      observationIds: NonEmptyList[Observation.Id]
    ): AppliedFragment =
      selectGmosMos("v_gmos_north_mos", observationIds)

    def selectGmosSouthMos(
      observationIds: NonEmptyList[Observation.Id]
    ): AppliedFragment =
      selectGmosMos("v_gmos_south_mos", observationIds)

    // The attachment type is written alongside the id to satisfy the foreign key.
    val InsertGmosNorthMos: Fragment[(
      Observation.Id,
      GmosNorthGrating,
      Option[GmosNorthFilter],
      GmosCustomSlitWidth,
      Option[Attachment.Id],
      GmosMosAcquisitionType,
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
      GmosCustomSlitWidth,
      Wavelength
    )] =
      sql"""
        INSERT INTO t_gmos_north_mos (
          c_observation_id,
          c_program_id,
          c_grating,
          c_filter,
          c_slit_width,
          c_mask_attachment_id,
          c_mask_attachment_type,
          c_acquisition_type,
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
          c_initial_slit_width,
          c_initial_central_wavelength
        )
        SELECT
          $observation_id,
          c_program_id,
          $gmos_north_grating,
          ${gmos_north_filter.opt},
          $gmos_custom_slit_width,
          ${attachment_id.opt},
          ${attachment_type.opt},
          $gmos_mos_acquisition_type,
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
          $gmos_custom_slit_width,
          $wavelength_pm
        FROM t_observation
        WHERE c_observation_id = $observation_id
       """.contramap { (o, g, l, sw, a, at, w, x, y, r, n, i, wd, so, ig, il, isw, iw) => (
         o, g, l, sw, a, a.as(AttachmentType.MosMask), at, w, x.map(_.value), y.map(_.value), r, n, i, wd, so, ig, il, isw, iw, o
       )}

    def insertGmosNorthMos(
      observationId: Observation.Id,
      input:         GmosMosInput.Create.North
    ): AppliedFragment =
      InsertGmosNorthMos.apply(
        observationId,
        input.grating,
        input.filter,
        input.customMask.slitWidth,
        maskAttachmentId(input.customMask),
        input.acquisitionType,
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
        input.customMask.slitWidth,
        input.common.centralWavelength
      )

    val InsertGmosSouthMos: Fragment[(
      Observation.Id,
      GmosSouthGrating,
      Option[GmosSouthFilter],
      GmosCustomSlitWidth,
      Option[Attachment.Id],
      GmosMosAcquisitionType,
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
      GmosCustomSlitWidth,
      Wavelength
    )] =
      sql"""
        INSERT INTO t_gmos_south_mos (
          c_observation_id,
          c_program_id,
          c_grating,
          c_filter,
          c_slit_width,
          c_mask_attachment_id,
          c_mask_attachment_type,
          c_acquisition_type,
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
          c_initial_slit_width,
          c_initial_central_wavelength
        )
        SELECT
          $observation_id,
          c_program_id,
          $gmos_south_grating,
          ${gmos_south_filter.opt},
          $gmos_custom_slit_width,
          ${attachment_id.opt},
          ${attachment_type.opt},
          $gmos_mos_acquisition_type,
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
          $gmos_custom_slit_width,
          $wavelength_pm
        FROM t_observation
        WHERE c_observation_id = $observation_id
       """.contramap { (o, g, l, sw, a, at, w, x, y, r, n, i, wd, so, ig, il, isw, iw) => (
         o, g, l, sw, a, a.as(AttachmentType.MosMask), at, w, x.map(_.value), y.map(_.value), r, n, i, wd, so, ig, il, isw, iw, o
       )}

    def insertGmosSouthMos(
      observationId: Observation.Id,
      input:         GmosMosInput.Create.South
    ): AppliedFragment =
      InsertGmosSouthMos.apply(
        observationId,
        input.grating,
        input.filter,
        input.customMask.slitWidth,
        maskAttachmentId(input.customMask),
        input.acquisitionType,
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
        input.customMask.slitWidth,
        input.common.centralWavelength
      )

    private def maskAttachmentId(mask: GmosFpuMask.Custom): Option[Attachment.Id] =
      mask.mask match
        case ToBeDefined => none
        case Defined(id) => id.some

    def deleteGmosNorthMos(
      which: List[Observation.Id]
    ): Option[AppliedFragment] =
      NonEmptyList.fromList(which).map: oids =>
        void"DELETE FROM ONLY t_gmos_north_mos " |+|
          void"WHERE " |+| observationIdIn(oids)

    def deleteGmosSouthMos(
      which: List[Observation.Id]
    ): Option[AppliedFragment] =
      NonEmptyList.fromList(which).map: oids =>
        void"DELETE FROM ONLY t_gmos_south_mos " |+|
          void"WHERE " |+| observationIdIn(oids)

    def commonUpdates(
      input: GmosMosInput.Edit.Common
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

    // The mask is replaced whole, so every column is assigned.
    // clearing the id must also clear the type.
    def customMaskUpdates(
      mask: GmosFpuMask.Custom
    ): List[AppliedFragment] =
      val upSlitWidth  = sql"c_slit_width           = $gmos_custom_slit_width"
      val upAttachment = sql"c_mask_attachment_id   = ${attachment_id.opt}"
      val upType       = sql"c_mask_attachment_type = ${attachment_type.opt}"

      val aid = maskAttachmentId(mask)
      List(
        upSlitWidth(mask.slitWidth),
        upAttachment(aid),
        upType(aid.as(AttachmentType.MosMask))
      )

    def gmosNorthUpdates(
      input: GmosMosInput.Edit.North
    ): Option[NonEmptyList[AppliedFragment]] = {

      val upGrating         = sql"c_grating         = $gmos_north_grating"
      val upFilter          = sql"c_filter          = ${gmos_north_filter.opt}"
      val upAcquisitionType = sql"c_acquisition_type = $gmos_mos_acquisition_type"

      val ups: List[AppliedFragment] =
        List(
          input.grating.map(upGrating),
          input.filter.toOptionOption.map(upFilter),
          input.acquisitionType.map(upAcquisitionType)
        ).flatten ++ input.customMask.toList.flatMap(customMaskUpdates) ++ commonUpdates(input.common)

      NonEmptyList.fromList(ups)
    }

    def updateGmosNorthMos(
      SET:   GmosMosInput.Edit.North,
      which: List[Observation.Id]
    ): Option[AppliedFragment] =
      for {
        us   <- gmosNorthUpdates(SET)
        oids <- NonEmptyList.fromList(which)
      } yield
        void"UPDATE t_gmos_north_mos " |+|
          void"SET " |+| us.intercalate(void", ") |+| void" " |+|
          void"WHERE " |+| observationIdIn(oids)

    def gmosSouthUpdates(
      input: GmosMosInput.Edit.South
    ): Option[NonEmptyList[AppliedFragment]] = {

      val upGrating         = sql"c_grating         = $gmos_south_grating"
      val upFilter          = sql"c_filter          = ${gmos_south_filter.opt}"
      val upAcquisitionType = sql"c_acquisition_type = $gmos_mos_acquisition_type"

      val ups: List[AppliedFragment] =
        List(
          input.grating.map(upGrating),
          input.filter.toOptionOption.map(upFilter),
          input.acquisitionType.map(upAcquisitionType)
        ).flatten ++ input.customMask.toList.flatMap(customMaskUpdates) ++ commonUpdates(input.common)

      NonEmptyList.fromList(ups)
    }

    def updateGmosSouthMos(
      SET:   GmosMosInput.Edit.South,
      which: List[Observation.Id]
    ): Option[AppliedFragment] =
      for {
        us   <- gmosSouthUpdates(SET)
        oids <- NonEmptyList.fromList(which)
      } yield
        void"UPDATE t_gmos_south_mos " |+|
          void"SET " |+| us.intercalate(void", ") |+| void" " |+|
          void"WHERE " |+| observationIdIn(oids)

    private def cloneGmosMos(
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
        c_slit_width,
        c_mask_attachment_id,
        c_mask_attachment_type,
        c_acquisition_type,
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
        c_initial_slit_width,
        c_initial_central_wavelength
      )
      SELECT
        $observation_id,
        (SELECT c_program_id FROM t_observation WHERE c_observation_id = $observation_id) AS c_program_id,
        c_observing_mode_type,
        c_grating,
        c_filter,
        c_slit_width,
        c_mask_attachment_id,
        c_mask_attachment_type,
        c_acquisition_type,
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
        c_initial_slit_width,
        c_initial_central_wavelength
      FROM #$table
      WHERE c_observation_id = $observation_id
      """.apply(newId, newId, originalId)

    def cloneGmosNorthMos(originalId: Observation.Id, newId: Observation.Id): AppliedFragment =
      cloneGmosMos("t_gmos_north_mos", originalId, newId)

    def cloneGmosSouthMos(originalId: Observation.Id, newId: Observation.Id): AppliedFragment =
      cloneGmosMos("t_gmos_south_mos", originalId, newId)

  }
}
