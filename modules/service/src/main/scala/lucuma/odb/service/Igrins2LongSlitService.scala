// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.all.*
import grackle.Result
import grackle.ResultT
import lucuma.core.enums.Igrins2OffsetMode
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.odb.data.ExposureTimeModeRole
import lucuma.odb.graphql.input.Igrins2LongSlitInput
import lucuma.odb.sequence.igrins2.longslit.Config
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.Igrins2Codecs.*
import skunk.*
import skunk.codec.boolean.bool
import skunk.implicits.*

import Services.Syntax.*

trait Igrins2LongSlitService[F[_]]:
  def select(
    which: List[Observation.Id]
  ): F[Map[Observation.Id, Config]]

  def insert(
    input: Igrins2LongSlitInput.Create,
    reqEtm: Option[ExposureTimeMode],
    which: List[Observation.Id]
  )(using Transaction[F]): F[Result[Unit]]

  def delete(which: List[Observation.Id])(using Transaction[F]): F[Unit]

  def update(
    SET: Igrins2LongSlitInput.Edit,
    which: List[Observation.Id]
  )(using Transaction[F]): F[Unit]

  def clone(originalId: Observation.Id, newId: Observation.Id): F[Unit]

object Igrins2LongSlitService:

  def instantiate[F[_]: {Concurrent as F, Services}]: Igrins2LongSlitService[F] =

    new Igrins2LongSlitService[F] {

      val igrins2LS: Decoder[Config] =
        (exposure_time_mode       *:
         igrins_2_offset_mode.opt *:
         igrins_2_offset_mode     *:
         bool.opt                 *:
         bool
        ).map { case (sci, offsetMode, offsetModeDefault, saveSVC, saveSVCDefault) =>
          Config(
            sci,
            offsetMode.getOrElse(offsetModeDefault),
            saveSVC.getOrElse(saveSVCDefault)
          )
        }

      override def select(
        which: List[Observation.Id]
      ): F[Map[Observation.Id, Config]] =
        NonEmptyList
          .fromList(which)
          .fold(List.empty.pure[F]): oids =>
            val af = Statements.selectIgrins2LongSlit(oids)
            session.prepareR(af.fragment.query(observation_id *: igrins2LS)).use: pq =>
              pq.stream(af.argument, chunkSize = 1024).compile.toList
          .map(_.toMap)

      private def insertExposureTimeModes(
        name:  String,
        input: Igrins2LongSlitInput.Create,
        req:   Option[ExposureTimeMode],
        which: List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        exposureTimeModeService
          .insertScienceOnlyWithDefaults(name, input.exposureTimeMode, req, which)
          .map(_.void)

      override def insert(
        input: Igrins2LongSlitInput.Create,
        req: Option[ExposureTimeMode],
        which: List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        (for
          _ <- ResultT(insertExposureTimeModes("IGRINS-2 Long Slit", input, req, which))
          _ <- ResultT.liftF(which.traverse { oid => session.exec(Statements.insertIgrins2LongSlit(oid, input)) }.void)
        yield ()).value

      def delete(which: List[Observation.Id])(using Transaction[F]): F[Unit] =
        Statements.deleteIgrins2(which).fold(F.unit)(session.exec)

      private def updateExposureTimeModes(
        input: Igrins2LongSlitInput.Edit,
        which: List[Observation.Id]
      )(using Transaction[F]): F[Unit] =
        input.exposureTimeMode.fold(().pure[F]): e =>
          services.exposureTimeModeService.updateMany(which, ExposureTimeModeRole.Science, e)

      override def update(
        SET: Igrins2LongSlitInput.Edit,
        which: List[Observation.Id]
      )(using Transaction[F]): F[Unit] =
        for
          _ <- updateExposureTimeModes(SET, which)
          _ <- Statements.updateIgrins2LongSlit(SET, which).fold(F.unit)(session.exec)
        yield ()

      def clone(originalId: Observation.Id, newId: Observation.Id): F[Unit] =
        session.exec(Statements.cloneIgrins2(originalId, newId))
    }

  object Statements {

    def selectIgrins2LongSlit(observationIds: NonEmptyList[Observation.Id]): AppliedFragment =
      sql"""
        SELECT
          ls.c_observation_id,
          sci.c_exposure_time_mode,
          sci.c_signal_to_noise_at,
          sci.c_signal_to_noise,
          sci.c_exposure_time,
          sci.c_exposure_count,
          ls.c_offset_mode,
          ls.c_offset_mode_default,
          ls.c_save_svc_images,
          ls.c_save_svc_images_default
        FROM
          t_igrins_2_long_slit ls
        LEFT JOIN t_exposure_time_mode sci
           ON sci.c_observation_id = ls.c_observation_id
          AND sci.c_role = 'science'
      """(Void) |+|
      void"""
        WHERE
          ls.c_observation_id IN ("""                                     |+|
            observationIds.map(sql"$observation_id").intercalate(void",") |+|
          void")"

    val InsertIgrins2LongSlit: Fragment[(
      Observation.Id,
      Option[Igrins2OffsetMode],
      Option[Boolean]
    )] =
      sql"""
        INSERT INTO t_igrins_2_long_slit (
          c_observation_id,
          c_program_id,
          c_offset_mode,
          c_save_svc_images
        )
        SELECT
          $observation_id,
          c_program_id,
          ${igrins_2_offset_mode.opt},
          ${bool.opt}
        FROM t_observation
        WHERE c_observation_id = $observation_id
       """.contramap { (o, m, s) => (o, m, s, o) }

    def insertIgrins2LongSlit(
      observationId: Observation.Id,
      input: Igrins2LongSlitInput.Create
    ): AppliedFragment =
      InsertIgrins2LongSlit.apply(
        observationId,
        input.explicitOffsetMode,
        input.explicitSaveSVCImages
      )

    def deleteIgrins2(which: List[Observation.Id]): Option[AppliedFragment] =
      NonEmptyList.fromList(which).map { oids =>
        void"DELETE FROM ONLY t_igrins_2_long_slit " |+|
          void"WHERE " |+| observationIdIn(oids)
      }

    private def igrins2Updates(input: Igrins2LongSlitInput.Edit): Option[NonEmptyList[AppliedFragment]] = {

      val upOffsetMode    = sql"c_offset_mode     = ${igrins_2_offset_mode.opt}"
      val upSaveSVCImages = sql"c_save_svc_images = ${bool.opt}"

      val ups: List[AppliedFragment] =
        List(
          input.explicitOffsetMode.toOptionOption.map(upOffsetMode),
          input.explicitSaveSVCImages.toOptionOption.map(upSaveSVCImages)
        ).flatten

      NonEmptyList.fromList(ups)
    }

    def updateIgrins2LongSlit(
      SET: Igrins2LongSlitInput.Edit,
      which: List[Observation.Id]
    ): Option[AppliedFragment] =
      for {
        us   <- igrins2Updates(SET)
        oids <- NonEmptyList.fromList(which)
      } yield
        void"UPDATE t_igrins_2_long_slit " |+|
          void"SET " |+| us.intercalate(void", ") |+| void" " |+|
          void"WHERE " |+| observationIdIn(oids)

    def cloneIgrins2(originalId: Observation.Id, newId: Observation.Id): AppliedFragment =
      sql"""
      INSERT INTO t_igrins_2_long_slit (
        c_observation_id,
        c_program_id,
        c_observing_mode_type,
        c_offset_mode,
        c_offset_mode_default,
        c_save_svc_images,
        c_save_svc_images_default
      )
      SELECT
        $observation_id,
        (SELECT c_program_id FROM t_observation WHERE c_observation_id = $observation_id) AS c_program_id,
        c_observing_mode_type,
        c_offset_mode,
        c_offset_mode_default,
        c_save_svc_images,
        c_save_svc_images_default
      FROM t_igrins_2_long_slit
      WHERE c_observation_id = $observation_id
      """.apply(newId, newId, originalId)
  }
