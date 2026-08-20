// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.all.*
import grackle.Result
import grackle.ResultT
import lucuma.core.enums.AttachmentType
import lucuma.core.enums.Flamingos2CustomSlitWidth
import lucuma.core.enums.Flamingos2Decker
import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2ReadMode
import lucuma.core.enums.Flamingos2ReadoutMode
import lucuma.core.enums.Flamingos2Reads
import lucuma.core.enums.SlitOffsetMode
import lucuma.core.model.Attachment
import lucuma.core.model.Defined
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.core.model.TelluricType
import lucuma.core.model.ToBeDefined
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.odb.data.ExposureTimeModeRole
import lucuma.odb.format.telescopeConfigs.*
import lucuma.odb.graphql.input.Flamingos2MosInput
import lucuma.odb.sequence.flamingos2.longslit.DefaultFlamingos2ReadoutMode
import lucuma.odb.sequence.flamingos2.mos.Config
import lucuma.odb.sequence.flamingos2.spectroscopy.AcquisitionConfig
import lucuma.odb.sequence.flamingos2.spectroscopy.Config.Common
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.Flamingos2Codecs.*
import skunk.*
import skunk.codec.text.text
import skunk.implicits.*

import Services.Syntax.*

trait Flamingos2MosService[F[_]]:

  def select(
    which: List[Observation.Id]
  ): F[Map[Observation.Id, Config]]

  def insert(
    input:  Flamingos2MosInput.Create,
    reqEtm: Option[ExposureTimeMode],
    which:  List[Observation.Id]
  )(using Transaction[F]): F[Result[Unit]]

  def delete(which: List[Observation.Id])(using Transaction[F]): F[Unit]

  def update(
    SET:   Flamingos2MosInput.Edit,
    which: List[Observation.Id]
  )(using Transaction[F]): F[Result[Unit]]

  def clone(originalId: Observation.Id, newId: Observation.Id): F[Unit]

object Flamingos2MosService:

  val MaskAttachmentViolationMessage: String =
    "The MOS mask attachment must exist, be of type 'mos_mask', and belong to the same program as the observation."

  def instantiate[F[_]: {Concurrent as F, Services}]: Flamingos2MosService[F] =

    new Flamingos2MosService[F]:

      val acquisition: Decoder[AcquisitionConfig] =
        (exposure_time_mode *:
         flamingos_2_filter *:
         flamingos_2_filter.opt
        ).to[AcquisitionConfig]

      val f2Mos: Decoder[Config] =
        (flamingos_2_disperser        *:
         flamingos_2_filter           *:
         flamingos_2_fpu_mask_custom  *:
         exposure_time_mode           *:
         acquisition                  *:
         flamingos_2_read_mode.opt    *:
         flamingos_2_reads.opt        *:
         flamingos_2_decker.opt       *:
         flamingos_2_readout_mode.opt *:
         slit_offset_mode             *: // c_slit_offset_mode_effective
         text                         *: // c_telescope_configs_effective
         telluric_type
        ).emap: (disperser, filter, mask, sci, acq, readMode, reads, decker, readoutMode, offsetMode, tcJson, telluricType) =>
          for
            tcs <- SlitTelescopeConfigsFormat
                     .getOption((offsetMode, tcJson))
                     .map(_.telescopeConfigs)
                     .toRight(s"Could not parse '$tcJson' as telescope configs.")
            cfg <- Config(
                     disperser,
                     filter,
                     mask,
                     acq,
                     Common(
                       sci,
                       readMode,
                       reads,
                       decker,
                       DefaultFlamingos2ReadoutMode,
                       readoutMode,
                       tcs,
                       telluricType
                     )
                   )
          yield cfg

      override def select(
        which: List[Observation.Id]
      ): F[Map[Observation.Id, Config]] =
        NonEmptyList
          .fromList(which)
          .fold(List.empty.pure[F]): oids =>
            val af = Statements.selectFlamingos2Mos(oids)
            session.prepareR(af.fragment.query(observation_id *: f2Mos)).use: pq =>
              pq.stream(af.argument, chunkSize = 1024).compile.toList
          .map(_.toMap)

      private def translateMaskViolation[A](fa: F[A]): F[Result[A]] =
        fa.map(Result.success).recover:
          case SqlState.ForeignKeyViolation(e) if e.constraintName.exists(_.contains("mask_attachment_fkey")) =>
            Result.failure(MaskAttachmentViolationMessage)

      override def insert(
        input: Flamingos2MosInput.Create,
        req:   Option[ExposureTimeMode],
        which: List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        (for
          _ <- ResultT(exposureTimeModeService.insertOneWithDefaults("Flamingos 2 MOS", input.acquisition.flatMap(_.exposureTimeMode), input.exposureTimeMode, req, which).map(_.void))
          _ <- ResultT(translateMaskViolation(which.traverse(oid => session.exec(Statements.insertFlamingos2Mos(oid, input))).void))
        yield ()).value

      override def delete(which: List[Observation.Id])(using Transaction[F]): F[Unit] =
        Statements.deleteFlamingos2Mos(which).fold(F.unit)(session.exec)

      override def update(
        SET:   Flamingos2MosInput.Edit,
        which: List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        def updateEtm(etm: Option[ExposureTimeMode], role: ExposureTimeModeRole): F[Unit] =
          etm.fold(F.unit): e =>
            services.exposureTimeModeService.updateMany(which, role, e)

        for
          _ <- updateEtm(SET.common.acquisition.flatMap(_.exposureTimeMode), ExposureTimeModeRole.Acquisition)
          _ <- updateEtm(SET.common.exposureTimeMode, ExposureTimeModeRole.Science)
          r <- translateMaskViolation(Statements.updateFlamingos2Mos(SET, which).fold(F.unit)(session.exec))
        yield r

      override def clone(originalId: Observation.Id, newId: Observation.Id): F[Unit] =
        session.exec(Statements.cloneFlamingos2Mos(originalId, newId))

  object Statements:

    def selectFlamingos2Mos(observationIds: NonEmptyList[Observation.Id]): AppliedFragment =
      sql"""
        SELECT
          m.c_observation_id,
          m.c_disperser,
          m.c_filter,
          m.c_mask_attachment_id,
          m.c_slit_width,
          sci.c_exposure_time_mode,
          sci.c_signal_to_noise_at,
          sci.c_signal_to_noise,
          sci.c_exposure_time,
          sci.c_exposure_count,
          acq.c_exposure_time_mode,
          acq.c_signal_to_noise_at,
          acq.c_signal_to_noise,
          acq.c_exposure_time,
          acq.c_exposure_count,
          m.c_acquisition_filter_default,
          m.c_acquisition_filter,
          m.c_read_mode,
          m.c_reads,
          m.c_decker,
          m.c_readout_mode,
          m.c_slit_offset_mode_effective,
          m.c_telescope_configs_effective,
          m.c_telluric_type
        FROM
          v_flamingos_2_mos m
        LEFT JOIN t_exposure_time_mode acq
           ON acq.c_observation_id = m.c_observation_id
          AND acq.c_role = 'acquisition'
        LEFT JOIN t_exposure_time_mode sci
           ON sci.c_observation_id = m.c_observation_id
          AND sci.c_role = 'science'
      """(Void) |+|
      void"""
        WHERE
          m.c_observation_id IN ("""                                     |+|
            observationIds.map(sql"$observation_id").intercalate(void",") |+|
          void")"

    // The attachment type is written alongside the id to satisfy the foreign key.
    val InsertFlamingos2Mos: Fragment[(
      Observation.Id,
      Flamingos2Disperser,
      Flamingos2Filter,
      Flamingos2CustomSlitWidth,
      Option[Attachment.Id],
      Option[Flamingos2Filter],
      Option[Flamingos2ReadMode],
      Option[Flamingos2Reads],
      Option[Flamingos2Decker],
      Option[Flamingos2ReadoutMode],
      Option[SlitOffsetMode],
      Option[String],
      TelluricType,
      Flamingos2Disperser,
      Flamingos2Filter,
      Flamingos2CustomSlitWidth
    )] =
      sql"""
        INSERT INTO t_flamingos_2_mos (
          c_observation_id,
          c_program_id,
          c_disperser,
          c_filter,
          c_slit_width,
          c_mask_attachment_id,
          c_mask_attachment_type,
          c_acquisition_filter,
          c_read_mode,
          c_reads,
          c_decker,
          c_readout_mode,
          c_slit_offset_mode,
          c_telescope_configs,
          c_telluric_type,
          c_initial_disperser,
          c_initial_filter,
          c_initial_slit_width
        )
        SELECT
          $observation_id,
          c_program_id,
          $flamingos_2_disperser,
          $flamingos_2_filter,
          $flamingos_2_custom_slit_width,
          ${attachment_id.opt},
          ${attachment_type.opt},
          ${flamingos_2_filter.opt},
          ${flamingos_2_read_mode.opt},
          ${flamingos_2_reads.opt},
          ${flamingos_2_decker.opt},
          ${flamingos_2_readout_mode.opt},
          ${slit_offset_mode.opt},
          ${text.opt},
          $telluric_type,
          $flamingos_2_disperser,
          $flamingos_2_filter,
          $flamingos_2_custom_slit_width
        FROM t_observation
        WHERE c_observation_id = $observation_id
       """.contramap { (o, d, f, sw, a, af, rm, rs, dk, ro, som, tc, tt, id, if_, isw) => (
         o, d, f, sw, a, a.as(AttachmentType.MosMask), af, rm, rs, dk, ro, som, tc, tt, id, if_, isw, o
       )}

    def insertFlamingos2Mos(
      observationId: Observation.Id,
      input:         Flamingos2MosInput.Create
    ): AppliedFragment =
      InsertFlamingos2Mos(
        observationId,
        input.disperser,
        input.filter,
        input.customMask.slitWidth,
        maskAttachmentId(input.customMask),
        input.acquisition.flatMap(_.filter.toOption),
        input.explicitReadMode,
        input.explicitReads,
        input.explicitDecker,
        input.explicitReadoutMode,
        input.explicitSlitOffsetMode,
        input.formattedTelescopeConfigs,
        input.telluricType,
        input.disperser,
        input.filter,
        input.customMask.slitWidth
      )

    private def maskAttachmentId(mask: Flamingos2FpuMask.Custom): Option[Attachment.Id] =
      mask.mask match
        case ToBeDefined => none
        case Defined(id) => id.some

    def deleteFlamingos2Mos(which: List[Observation.Id]): Option[AppliedFragment] =
      NonEmptyList.fromList(which).map: oids =>
        void"DELETE FROM ONLY t_flamingos_2_mos " |+|
          void"WHERE " |+| observationIdIn(oids)

    // The mask is replaced whole, so every column is assigned.
    // clearing the id must also clear the type.
    private def customMaskUpdates(mask: Flamingos2FpuMask.Custom): List[AppliedFragment] =
      val upSlitWidth  = sql"c_slit_width           = $flamingos_2_custom_slit_width"
      val upAttachment = sql"c_mask_attachment_id   = ${attachment_id.opt}"
      val upType       = sql"c_mask_attachment_type = ${attachment_type.opt}"

      val aid = maskAttachmentId(mask)
      List(
        upSlitWidth(mask.slitWidth),
        upAttachment(aid),
        upType(aid.as(AttachmentType.MosMask))
      )

    private def mosUpdates(input: Flamingos2MosInput.Edit): Option[NonEmptyList[AppliedFragment]] =

      val upDisperser     = sql"c_disperser           = $flamingos_2_disperser"
      val upFilter        = sql"c_filter              = $flamingos_2_filter"
      val upAcqFilter     = sql"c_acquisition_filter  = ${flamingos_2_filter.opt}"
      val upReadMode      = sql"c_read_mode           = ${flamingos_2_read_mode.opt}"
      val upReads         = sql"c_reads               = ${flamingos_2_reads.opt}"
      val upDecker        = sql"c_decker              = ${flamingos_2_decker.opt}"
      val upReadoutMode   = sql"c_readout_mode        = ${flamingos_2_readout_mode.opt}"
      val upSlitMode      = sql"c_slit_offset_mode    = ${slit_offset_mode.opt}"
      val upTelescopeCfgs = sql"c_telescope_configs   = ${text.opt}"
      val upTelluricType  = sql"c_telluric_type       = $telluric_type"

      val common = input.common

      val ups: List[AppliedFragment] =
        List(
          input.disperser.map(upDisperser),
          input.filter.map(upFilter),
          common.acquisition.flatMap(_.filter.toOptionOption).map(upAcqFilter),
          common.explicitReadMode.toOptionOption.map(upReadMode),
          common.explicitReads.toOptionOption.map(upReads),
          common.explicitDecker.toOptionOption.map(upDecker),
          common.explicitReadoutMode.toOptionOption.map(upReadoutMode),
          common.explicitSlitOffsetMode.toOptionOption.map(upSlitMode),
          common.formattedTelescopeConfigs.toOptionOption.map(upTelescopeCfgs),
          common.telluricType.map(upTelluricType)
        ).flatten ++ input.customMask.toList.flatMap(customMaskUpdates)

      NonEmptyList.fromList(ups)

    def updateFlamingos2Mos(
      SET:   Flamingos2MosInput.Edit,
      which: List[Observation.Id]
    ): Option[AppliedFragment] =
      for
        us   <- mosUpdates(SET)
        oids <- NonEmptyList.fromList(which)
      yield
        void"UPDATE t_flamingos_2_mos " |+|
          void"SET " |+| us.intercalate(void", ") |+| void" " |+|
          void"WHERE " |+| observationIdIn(oids)

    def cloneFlamingos2Mos(originalId: Observation.Id, newId: Observation.Id): AppliedFragment =
      sql"""
      INSERT INTO t_flamingos_2_mos (
        c_observation_id,
        c_program_id,
        c_observing_mode_type,
        c_disperser,
        c_filter,
        c_slit_width,
        c_mask_attachment_id,
        c_mask_attachment_type,
        c_acquisition_filter,
        c_read_mode,
        c_reads,
        c_decker,
        c_decker_default,
        c_readout_mode,
        c_readout_mode_default,
        c_slit_offset_mode,
        c_telescope_configs,
        c_telluric_type,
        c_initial_disperser,
        c_initial_filter,
        c_initial_slit_width
      )
      SELECT
        $observation_id,
        (SELECT c_program_id FROM t_observation WHERE c_observation_id = $observation_id) AS c_program_id,
        c_observing_mode_type,
        c_disperser,
        c_filter,
        c_slit_width,
        c_mask_attachment_id,
        c_mask_attachment_type,
        c_acquisition_filter,
        c_read_mode,
        c_reads,
        c_decker,
        c_decker_default,
        c_readout_mode,
        c_readout_mode_default,
        c_slit_offset_mode,
        c_telescope_configs,
        c_telluric_type,
        c_initial_disperser,
        c_initial_filter,
        c_initial_slit_width
      FROM t_flamingos_2_mos
      WHERE c_observation_id = $observation_id
      """.apply(newId, newId, originalId)
