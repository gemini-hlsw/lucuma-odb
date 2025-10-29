// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.all.*
import grackle.Result
import grackle.ResultT
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.Flamingos2Decker
import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2Fpu
import lucuma.core.enums.Flamingos2ReadMode
import lucuma.core.enums.Flamingos2ReadoutMode
import lucuma.core.enums.Flamingos2Reads
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.core.model.TelluricType
import lucuma.odb.data.ExposureTimeModeRole
import lucuma.odb.format.spatialOffsets.*
import lucuma.odb.graphql.input.Flamingos2LongSlitInput
import lucuma.odb.json.all.query.given
import lucuma.odb.sequence.flamingos2.longslit.Config
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.Flamingos2Codecs.*
import skunk.*
import skunk.circe.codec.json.*
import skunk.codec.text.text
import skunk.implicits.*

import Services.Syntax.*

trait Flamingos2LongSlitService[F[_]]:
  def select(
    which: List[Observation.Id]
  ): F[Map[Observation.Id, Config]]

  def insert(
    input:  Flamingos2LongSlitInput.Create,
    reqEtm: Option[ExposureTimeMode],
    which:  List[Observation.Id]
  )(using Transaction[F]): F[Result[Unit]]

  def delete(which: List[Observation.Id])(using Transaction[F]): F[Unit]

  def update(
    SET:   Flamingos2LongSlitInput.Edit,
    which: List[Observation.Id]
  )(using Transaction[F]): F[Unit]

  def clone(originalId: Observation.Id, newId: Observation.Id): F[Unit]

object Flamingos2LongSlitService:

  def instantiate[F[_]: {Concurrent as F, Services}]: Flamingos2LongSlitService[F] =

    new Flamingos2LongSlitService[F] {

      val f2LS: Decoder[Config] =
        (flamingos_2_disperser        *:
         flamingos_2_filter           *:
         flamingos_2_fpu              *:
         exposure_time_mode           *:
         exposure_time_mode           *:
         flamingos_2_read_mode.opt    *:
         flamingos_2_reads.opt        *:
         flamingos_2_decker.opt       *:
         flamingos_2_readout_mode.opt *:
         text.opt                     *:
         jsonb
        ).emap { case (disperser, filter, fpu, acq, sci, readMode, reads, decker, readoutMode, offsetsText, telluricTypeJson) =>
          (offsetsText.traverse: so =>
            OffsetsFormat.getOption(so).toRight(s"Could not parse '$so' as an offsets list."),
            telluricTypeJson.as[TelluricType].leftMap(_.getMessage)).mapN { (offsets, telluricType) =>
              Config(disperser, filter, fpu, acq, sci, readMode, reads, decker, readoutMode, offsets, telluricType)
          }
        }

      override def select(
        which: List[Observation.Id]
      ): F[Map[Observation.Id, Config]] =
        NonEmptyList
          .fromList(which)
          .fold(List.empty.pure[F]): oids =>
            val af = Statements.selectFlamingos2LongSlit(oids)
            session.prepareR(af.fragment.query(observation_id *: f2LS)).use: pq =>
              pq.stream(af.argument, chunkSize = 1024).compile.toList
          .map(_.toMap)

      private def insertExposureTimeModes(
        name:  String,
        input: Flamingos2LongSlitInput.Create,
        req:   Option[ExposureTimeMode],
        which: List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        exposureTimeModeService.insertOneWithDefaults(
          name,
          input.acquisition.flatMap(_.exposureTimeMode),
          input.exposureTimeMode,
          req,
          which
        ).map(_.void)

      override def insert(
        input: Flamingos2LongSlitInput.Create,
        req:   Option[ExposureTimeMode],
        which: List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        (for
          _ <- ResultT(insertExposureTimeModes("Flamingos 2 Long Slit", input, req, which))
          _ <- ResultT.liftF(which.traverse { oid => session.exec(Statements.insertF2LongSlit(oid, input)) }.void)
        yield ()).value

      def delete(which: List[Observation.Id])(using Transaction[F]): F[Unit] =
        Statements.deleteF2(which).fold(F.unit)(session.exec)

      private def updateExposureTimeModes(
        input: Flamingos2LongSlitInput.Edit,
        which: List[Observation.Id]
      )(using Transaction[F]): F[Unit] =

        def update(etm: Option[ExposureTimeMode], role: ExposureTimeModeRole): F[Unit] =
          etm.fold(().pure[F]): e =>
            services.exposureTimeModeService.updateMany(which, role, e)

        for
          _ <- update(input.acquisition.flatMap(_.exposureTimeMode), ExposureTimeModeRole.Acquisition)
          _ <- update(input.exposureTimeMode, ExposureTimeModeRole.Science)
        yield ()

      override def update(
        SET: Flamingos2LongSlitInput.Edit,
        which: List[Observation.Id]
      )(using Transaction[F]): F[Unit] =
        for
          _ <- updateExposureTimeModes(SET, which)
          _ <- Statements.updateF2LongSlit(SET, which).fold(F.unit)(session.exec)
        yield ()

      def clone(originalId: Observation.Id, newId: Observation.Id): F[Unit] =
        session.exec(Statements.cloneF2(originalId, newId))
    }

  object Statements {

    def selectFlamingos2LongSlit(observationIds: NonEmptyList[Observation.Id]): AppliedFragment =
      sql"""
        SELECT
          ls.c_observation_id,
          ls.c_disperser,
          ls.c_filter,
          ls.c_fpu,
          acq.c_exposure_time_mode,
          acq.c_signal_to_noise_at,
          acq.c_signal_to_noise,
          acq.c_exposure_time,
          acq.c_exposure_count,
          sci.c_exposure_time_mode,
          sci.c_signal_to_noise_at,
          sci.c_signal_to_noise,
          sci.c_exposure_time,
          sci.c_exposure_count,
          ls.c_read_mode,
          ls.c_reads,
          ls.c_decker,
          ls.c_readout_mode,
          ls.c_offsets,
          ls.c_telluric_type
        FROM
          t_flamingos_2_long_slit ls
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

    val InsertF2LongSlit: Fragment[(
      Observation.Id               ,
      Flamingos2Disperser          ,
      Flamingos2Filter             ,
      Flamingos2Fpu                ,
      Option[Flamingos2ReadMode]   ,
      Option[Flamingos2Reads]      ,
      Option[Flamingos2Decker]     ,
      Option[Flamingos2ReadoutMode],
      Option[String]               ,
      TelluricType                 ,
      Flamingos2Disperser          ,
      Flamingos2Filter             ,
      Flamingos2Fpu
    )] =
      sql"""
        INSERT INTO t_flamingos_2_long_slit (
          c_observation_id,
          c_program_id,
          c_disperser,
          c_filter,
          c_fpu,
          c_read_mode,
          C_reads,
          c_decker,
          c_readout_mode,
          c_offsets,
          c_telluric_type,
          c_initial_disperser,
          c_initial_filter,
          c_initial_fpu
        )
        SELECT
          $observation_id,
          c_program_id,
          $flamingos_2_disperser,
          $flamingos_2_filter,
          $flamingos_2_fpu,
          ${flamingos_2_read_mode.opt},
          ${flamingos_2_reads.opt},
          ${flamingos_2_decker.opt},
          ${flamingos_2_readout_mode.opt},
          ${text.opt},
          $jsonb,
          $flamingos_2_disperser,
          $flamingos_2_filter,
          $flamingos_2_fpu
        FROM t_observation
        WHERE c_observation_id = $observation_id
       """.contramap { (o, d, f, u, r, e, m, a, s, t, id, ii, iu) => (o, d, f, u, r, e, m, a, s, t.asJson, id, ii, iu, o)}

    def insertF2LongSlit(
      observationId: Observation.Id,
      input:         Flamingos2LongSlitInput.Create
    ): AppliedFragment =
      InsertF2LongSlit.apply(
        observationId                ,
        input.disperser              ,
        input.filter                 ,
        input.fpu                    ,
        input.explicitReadMode       ,
        input.explicitReads          ,
        input.explicitDecker         ,
        input.explicitReadoutMode    ,
        input.formattedOffsets       ,
        input.telluricType           ,
        input.disperser              ,
        input.filter                 ,
        input.fpu                    ,
      )

    def deleteF2(which: List[Observation.Id]): Option[AppliedFragment] =
      NonEmptyList.fromList(which).map { oids =>
        void"DELETE FROM ONLY t_flamingos_2_long_slit " |+|
          void"WHERE " |+| observationIdIn(oids)
      }

    private def f2Updates(input: Flamingos2LongSlitInput.Edit): Option[NonEmptyList[AppliedFragment]] = {

      val upDisperser      = sql"c_disperser       = $flamingos_2_disperser"
      val upFilter         = sql"c_filter          = $flamingos_2_filter"
      val upFpu            = sql"c_fpu             = $flamingos_2_fpu"
      val upReadMode       = sql"c_read_mode       = ${flamingos_2_read_mode.opt}"
      val upReads          = sql"c_reads           = ${flamingos_2_reads.opt}"
      val upDecker         = sql"c_decker          = ${flamingos_2_decker.opt}"
      val upReadoutMode    = sql"c_readout_mode    = ${flamingos_2_readout_mode.opt}"
      val upOffsets        = sql"c_offsets         = ${text.opt}"
      val upTelluricType   = sql"c_telluric_type   = ${jsonb.opt}"

      val ups: List[AppliedFragment] =
        List(
          input.disperser.map(upDisperser),
          input.filter.map(upFilter),
          input.fpu.map(upFpu),
          input.explicitReadMode.toOptionOption.map(upReadMode),
          input.explicitReads.toOptionOption.map(upReads),
          input.explicitDecker.toOptionOption.map(upDecker),
          input.explicitReadoutMode.toOptionOption.map(upReadoutMode),
          input.formattedOffsets.toOptionOption.map(upOffsets),
          input.telluricType.map(tt => upTelluricType(Some(tt.asJson)))
        ).flatten

      NonEmptyList.fromList(ups)
    }

    def updateF2LongSlit(
      SET:   Flamingos2LongSlitInput.Edit,
      which: List[Observation.Id]
    ): Option[AppliedFragment] =
      for {
        us   <- f2Updates(SET)
        oids <- NonEmptyList.fromList(which)
      } yield
        void"UPDATE t_flamingos_2_long_slit " |+|
          void"SET " |+| us.intercalate(void", ") |+| void" " |+|
          void"WHERE " |+| observationIdIn(oids)

    def cloneF2(originalId: Observation.Id, newId: Observation.Id): AppliedFragment =
      sql"""
      INSERT INTO t_flamingos_2_long_slit (
        c_observation_id,
        c_program_id,
        c_observing_mode_type,
        c_disperser,
        c_filter,
        c_fpu,
        c_read_mode,
        c_reads,
        c_decker,
        c_decker_default,
        c_readout_mode,
        c_readout_mode_default,
        c_offsets,
        c_telluric_type,
        c_initial_disperser,
        c_initial_filter,
        c_initial_fpu
      )
      SELECT
        $observation_id,
        (SELECT c_program_id FROM t_observation WHERE c_observation_id = $observation_id) AS c_program_id,
        c_observing_mode_type,
        c_disperser,
        c_filter,
        c_fpu,
        c_read_mode,
        c_reads,
        c_decker,
        c_decker_default,
        c_readout_mode,
        c_readout_mode_default,
        c_offsets,
        c_telluric_type,
        c_initial_disperser,
        c_initial_filter,
        c_initial_fpu
      FROM t_flamingos_2_long_slit
      WHERE c_observation_id = $observation_id
      """.apply(newId, newId, originalId)
  }