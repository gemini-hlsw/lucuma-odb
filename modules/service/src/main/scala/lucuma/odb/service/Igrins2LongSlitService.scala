// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.all.*
import grackle.Result
import grackle.ResultT
import lucuma.core.enums.SlitOffsetMode
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.core.model.TelluricType
import lucuma.core.util.TimeSpan
import lucuma.odb.data.ExposureTimeModeRole
import lucuma.odb.format.telescopeConfigs.*
import lucuma.odb.graphql.input.Igrins2LongSlitInput
import lucuma.odb.sequence.igrins2.longslit.Config
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.codec.boolean.bool
import skunk.codec.text.text
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

  /** Reset the configuration of `oid` to telluric `defaults.
    */
  def resetTelluricConfig(oid: Observation.Id): F[Unit]

object Igrins2LongSlitService:

  def instantiate[F[_]: {Concurrent as F, Services}]: Igrins2LongSlitService[F] =

    new Igrins2LongSlitService[F] {

      val igrins2LS: Decoder[Config] =
        (exposure_time_mode  *:
         bool                *:
         slit_offset_mode    *:
         text                *:
         telluric_type
        ).emap { case (sci, saveSVC, offsetMode, configsText, telluricType) =>
          SlitTelescopeConfigsFormat
            .getOption((offsetMode, configsText))
            .toRight(s"Could not parse '$configsText' as telescope configs (mode ${offsetMode.tag}).")
            .map: configs =>
              Config(sci, saveSVC, configs, telluricType)
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

      override def resetTelluricConfig(oid: Observation.Id): F[Unit] =
        session.exec(Statements.applyIgrins2TelluricDefaults(oid))
    }

  object Statements {

    def selectIgrins2LongSlit(observationIds: NonEmptyList[Observation.Id]): AppliedFragment =
      sql"""
        SELECT
          ls.c_observation_id     ,
          sci.c_exposure_time_mode,
          sci.c_signal_to_noise_at,
          sci.c_signal_to_noise   ,
          sci.c_exposure_time     ,
          sci.c_exposure_count    ,
          ls.c_save_svc_images    ,
          ls.c_slit_offset_mode_effective ,
          ls.c_telescope_configs_effective,
          ls.c_telluric_type
        FROM
          v_igrins_2_long_slit ls
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
      Option[Boolean],
      Option[TimeSpan],
      Option[String],
      Option[SlitOffsetMode],
      Option[String],
      TelluricType
    )] =
      sql"""
        INSERT INTO t_igrins_2_long_slit (
          c_observation_id       ,
          c_program_id           ,
          c_save_svc_images      ,
          c_svc_exposure         ,
          c_svc_telescope_configs,
          c_slit_offset_mode     ,
          c_telescope_configs    ,
          c_telluric_type
        )
        SELECT
          $observation_id        ,
          c_program_id           ,
          ${bool.opt}            ,
          ${time_span.opt}       ,
          ${text.opt}            ,
          ${slit_offset_mode.opt},
          ${text.opt}            ,
          $telluric_type
        FROM t_observation
        WHERE c_observation_id = $observation_id
       """.contramap { (o, s, e, st, m, tc, tt) => (o, s, e, st, m, tc, tt, o) }

    def insertIgrins2LongSlit(
      observationId: Observation.Id,
      input:         Igrins2LongSlitInput.Create
    ): AppliedFragment =
      // An empty explicit SVC telescope-configs list reverts to the default.
      val svcTelescopeConfigs =
        input.svc.flatMap(_.explicitTelescopeConfigs).flatMap(NonEmptyList.fromList).map(ToSkyFormat.reverseGet)
      InsertIgrins2LongSlit.apply(
        observationId,
        Some(input.svc.isDefined),
        input.svc.flatMap(_.explicitExposure),
        svcTelescopeConfigs,
        input.explicitSlitOffsetMode,
        input.formattedTelescopeConfigs,
        input.telluricType
      )

    def deleteIgrins2(which: List[Observation.Id]): Option[AppliedFragment] =
      NonEmptyList.fromList(which).map { oids =>
        void"DELETE FROM ONLY t_igrins_2_long_slit " |+|
          void"WHERE " |+| observationIdIn(oids)
      }

    private def igrins2Updates(input: Igrins2LongSlitInput.Edit): Option[NonEmptyList[AppliedFragment]] = {

      val upSaveSVCImages   = sql"c_save_svc_images       = ${bool.opt}"
      val upSvcExposure     = sql"c_svc_exposure          = ${time_span.opt}"
      val upSvcTelescope    = sql"c_svc_telescope_configs = ${text.opt}"
      val upSlitOffsetMode  = sql"c_slit_offset_mode      = ${slit_offset_mode.opt}"
      val upTelescopeConfig = sql"c_telescope_configs     = ${text.opt}"
      val upTelluricType    = sql"c_telluric_type         = ${telluric_type.opt}"

      // c_save_svc_images: svc omitted -> skip; svc:null -> OFF (false); svc:{...} -> ON (true).
      val saveSvc: Option[Option[Boolean]] =
        input.svc.fold(Some(Some(false)), None, _ => Some(Some(true)))

      // SVC exposure / telescope configs apply only when svc is a non-null object (ON/edit).
      val svcParams: Option[Igrins2LongSlitInput.Svc.Edit] = input.svc.toOption

      val svcExposure: Option[Option[TimeSpan]] =
        svcParams.flatMap(_.explicitExposure.toOptionOption)

      // An empty explicit SVC telescope-configs list reverts to the default.
      val svcTelescopeConfigs: Option[Option[String]] =
        svcParams.flatMap(_.explicitTelescopeConfigs.toOptionOption)
          .map(_.flatMap(tcs => NonEmptyList.fromList(tcs).map(ToSkyFormat.reverseGet)))

      val ups: List[AppliedFragment] =
        List(
          saveSvc.map(upSaveSVCImages),
          svcExposure.map(upSvcExposure),
          svcTelescopeConfigs.map(upSvcTelescope),
          input.explicitSlitOffsetMode.toOptionOption.map(upSlitOffsetMode),
          input.formattedTelescopeConfigs.toOptionOption.map(upTelescopeConfig),
          input.telluricType.map(tt => upTelluricType(Some(tt)))
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

    // Tellurics need a fixed set of telescope configs
    def applyIgrins2TelluricDefaults(oid: Observation.Id): AppliedFragment =
      val (mode, configs) = SlitTelescopeConfigsFormat.reverseGet(Config.DefaultTelescopeConfigs)
      sql"""
        UPDATE t_igrins_2_long_slit
        SET
          c_slit_offset_mode  = $slit_offset_mode,
          c_telescope_configs = $text
        WHERE c_observation_id = $observation_id
      """.apply(mode, configs, oid)

    def cloneIgrins2(originalId: Observation.Id, newId: Observation.Id): AppliedFragment =
      sql"""
      INSERT INTO t_igrins_2_long_slit (
        c_observation_id       ,
        c_program_id           ,
        c_observing_mode_type  ,
        c_save_svc_images      ,
        c_svc_exposure         ,
        c_svc_telescope_configs,
        c_slit_offset_mode     ,
        c_telescope_configs    ,
        c_telluric_type
      )
      SELECT
        $observation_id,
        (SELECT c_program_id FROM t_observation WHERE c_observation_id = $observation_id) AS c_program_id,
        c_observing_mode_type,
        c_save_svc_images,
        c_svc_exposure,
        c_svc_telescope_configs,
        c_slit_offset_mode,
        c_telescope_configs,
        c_telluric_type
      FROM t_igrins_2_long_slit
      WHERE c_observation_id = $observation_id
      """.apply(newId, newId, originalId)
  }
