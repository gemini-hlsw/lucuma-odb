// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Concurrent
import eu.timepit.refined.types.numeric.PosInt
import cats.syntax.all.*
import grackle.Result
import grackle.ResultT
import lucuma.core.enums.ScorpioFilter
import lucuma.core.enums.ScorpioFpu
import lucuma.core.enums.SlitOffsetMode
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.odb.data.ExposureTimeModeId
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.format.telescopeConfigs.*
import lucuma.odb.graphql.input.ScorpioLongSlitInput
import lucuma.odb.sequence.ObservingMode
import lucuma.odb.sequence.scorpio.longslit.Config
import lucuma.odb.syntax.exposureTimeMode.*
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.ScorpioCodecs.*
import skunk.*
import skunk.codec.text.text
import skunk.data.Completion
import skunk.implicits.*

import Services.Syntax.*

trait ScorpioLongSlitService[F[_]]:
  def select(
    which: List[Observation.Id]
  ): F[Map[Observation.Id, Config]]

  def insert(
    input:  ScorpioLongSlitInput.Create,
    reqEtm: Option[ExposureTimeMode],
    which:  List[Observation.Id]
  )(using Transaction[F]): F[Result[Unit]]

  def delete(
    which: List[Observation.Id]
  )(using Transaction[F]): F[Unit]

  def update(
    SET:   ScorpioLongSlitInput.Edit,
    which: List[Observation.Id]
  )(using Transaction[F]): F[Result[Unit]]

  def clone(
    originalId: Observation.Id,
    newId:      Observation.Id,
    etms:       List[(ExposureTimeModeId, ExposureTimeModeId)]
  )(using Transaction[F]): F[Unit]

object ScorpioLongSlitService:

  private type ChannelModes = List[(ScorpioFilter, ExposureTimeMode)]

  // Channels read out by one detector controller cannot be exposed differently.
  private val SharedControllers: List[(ScorpioFilter, ScorpioFilter)] =
    List(ScorpioFilter.G -> ScorpioFilter.R, ScorpioFilter.I -> ScorpioFilter.Z)

  private def controllerPartner(channel: ScorpioFilter): Option[ScorpioFilter] =
    SharedControllers.collectFirst:
      case (`channel`, b) => b
      case (a, `channel`) => a

  private object validateEtms:

    private def failure(msg: String): Result[Unit] =
      OdbError.InvalidArgument(msg.some).asFailure

    private def timeAndCount(modes: ChannelModes): Result[Unit] =
      failure(s"${ObservingMode.ScorpioLongSlitName} requires time and count exposure time modes.")
        .unlessA(modes.forall((_, etm) => ExposureTimeMode.timeAndCount.getOption(etm).isDefined))

    private def sharedController(modes: ChannelModes, a: ScorpioFilter, b: ScorpioFilter): Result[Unit] =
      def exposure(f: ScorpioFilter) =
        modes
          .collectFirst { case (`f`, etm) => etm }
          .flatMap(ExposureTimeMode.timeAndCount.getOption)
          .map(tc => (tc.time, tc.count.value))

      failure(s"The SCORPIO ${a.shortName} and ${b.shortName} channels share a detector controller and must have the same exposure time and count.")
        .unlessA((exposure(a), exposure(b)).mapN(_ === _).getOrElse(true))

    // A channel missing from `modes` is not compared with its partner.
    def apply(modes: ChannelModes): Result[Unit] =
      timeAndCount(modes) *>
        SharedControllers.traverse_((a, b) => sharedController(modes, a, b))

  def instantiate[F[_]: {Concurrent as F, Services}]: ScorpioLongSlitService[F] =

    new ScorpioLongSlitService[F]:

      override def select(
        which: List[Observation.Id]
      ): F[Map[Observation.Id, Config]] =
        NonEmptyList
          .fromList(which)
          .fold(List.empty.pure[F]): oids =>
            val af = Statements.select(oids)
            session.prepareR(af.fragment.query(observation_id *: Statements.scorpio_long_slit)).use: pq =>
              pq.stream(af.argument, chunkSize = 1024).compile.toList
          .map(_.toMap)

      override def insert(
        input:  ScorpioLongSlitInput.Create,
        reqEtm: Option[ExposureTimeMode],
        which:  List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        NonEmptyList
          .fromList(which)
          .fold(ResultT.unit[F]): oids =>
            for
              r   <- ResultT(exposureTimeModeService.resolve(ObservingMode.ScorpioLongSlitName, none, input.exposureTimeModes, reqEtm, which))
              sci  = r.view.mapValues(_._2).toMap
              _   <- ResultT.fromResult(sci.values.toList.traverse_(m => validateEtms(m.toList)))
              ids <- ResultT.liftF(exposureTimeModeService.insertResolvedScienceOnly(sci))
              _   <- ResultT.liftF:
                       oids.traverse_ : oid =>
                         session.exec(Statements.insert(oid, input, ids(oid).toList.toMap))
            yield ()
          .value

      override def delete(
        which: List[Observation.Id]
      )(using Transaction[F]): F[Unit] =
        Statements.delete(which).traverse_(session.exec)

      override def update(
        SET:   ScorpioLongSlitInput.Edit,
        which: List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        val modes = SET.exposureTimeModes

        // A channel edited alone takes its controller partner along.
        val partners: List[(ScorpioFilter, ExposureTimeMode)] =
          modes.flatMap: (channel, etm) =>
            controllerPartner(channel)
              .filterNot(p => modes.exists(_._1 === p))
              .tupleRight(etm)

        NonEmptyList
          .fromList(which)
          .fold(ResultT.unit[F]): oids =>
            for
              _ <- ResultT.fromResult(validateEtms(modes))
              _ <- ResultT.liftF(Statements.update(SET, oids).traverse_(session.exec))
              _ <- ResultT.liftF(modes.traverse_((c, etm) => session.exec(Statements.updateEtm(oids, c, etm))))
              _ <- ResultT.liftF(partners.traverse_((c, etm) => session.exec(Statements.updateExposure(oids, c, etm))))
            yield ()
          .value

      override def clone(
        originalId: Observation.Id,
        newId:      Observation.Id,
        etms:       List[(ExposureTimeModeId, ExposureTimeModeId)]
      )(using Transaction[F]): F[Unit] =
        session
          .executeCommand(Statements.clone(originalId, newId, etms))
          .flatMap:
            case Completion.Insert(1) =>
              F.unit
            case _                    =>
              F.raiseError(new RuntimeException(s"Could not clone ${ObservingMode.ScorpioLongSlitName} observing mode $originalId, $newId"))

  object Statements:

    val scorpio_long_slit: Decoder[Config] =
      (
        scorpio_fpu        *:
        exposure_time_mode *:
        exposure_time_mode *:
        exposure_time_mode *:
        exposure_time_mode *:
        slit_offset_mode   *:
        text               *:
        int4_pos
      ).emap: (fpu, g, r, i, z, offsetMode, configsText, repeats) =>
        SlitTelescopeConfigsFormat
          .getOption((offsetMode, configsText))
          .toRight(s"Could not parse '$configsText' as telescope configs (mode ${offsetMode.tag}).")
          .map(Config(fpu, g, r, i, z, _, repeats))

    private def etmIdColumn(channel: ScorpioFilter): String =
      s"c_${channel.tag.toLowerCase}_exposure_time_mode_id"

    private def etmColumns(channel: ScorpioFilter): String =
      val alias = channel.tag.toLowerCase
      List(
        "c_exposure_time_mode",
        "c_signal_to_noise_at",
        "c_signal_to_noise",
        "c_exposure_time",
        "c_exposure_count"
      ).map(c => s"$alias.$c").mkString(",\n")

    private def etmJoin(channel: ScorpioFilter): String =
      val alias = channel.tag.toLowerCase
      s"JOIN t_exposure_time_mode $alias ON $alias.c_exposure_time_mode_id = ls.${etmIdColumn(channel)}"

    def select(
      which: NonEmptyList[Observation.Id]
    ): AppliedFragment =
      sql"""
        SELECT
          ls.c_observation_id,
          ls.c_fpu,
          #${ScorpioFilter.visible.toList.map(etmColumns).mkString(",\n")},
          ls.c_slit_offset_mode_effective,
          ls.c_telescope_configs_effective,
          ls.c_repeats
        FROM
          v_scorpio_long_slit ls
        #${ScorpioFilter.visible.toList.map(etmJoin).mkString("\n")}
        WHERE
          ls.c_observation_id IN ("""(Void)                      |+|
            which.map(sql"$observation_id").intercalate(void",") |+|
          void")"

    private val Insert: Fragment[(
      Observation.Id,
      ScorpioFpu,
      ExposureTimeModeId,
      ExposureTimeModeId,
      ExposureTimeModeId,
      ExposureTimeModeId,
      Option[SlitOffsetMode],
      Option[String],
      PosInt
    )] =
      sql"""
        INSERT INTO t_scorpio_long_slit (
          c_observation_id,
          c_program_id,
          c_fpu,
          c_g_exposure_time_mode_id,
          c_r_exposure_time_mode_id,
          c_i_exposure_time_mode_id,
          c_z_exposure_time_mode_id,
          c_slit_offset_mode,
          c_telescope_configs,
          c_repeats
        )
        SELECT
          $observation_id,
          c_program_id,
          $scorpio_fpu,
          $exposure_time_mode_id,
          $exposure_time_mode_id,
          $exposure_time_mode_id,
          $exposure_time_mode_id,
          ${slit_offset_mode.opt},
          ${text.opt},
          $int4_pos
        FROM t_observation
        WHERE c_observation_id = $observation_id
      """.contramap { (o, f, g, r, i, z, m, tc, n) => (o, f, g, r, i, z, m, tc, n, o) }

    def insert(
      observationId: Observation.Id,
      input:         ScorpioLongSlitInput.Create,
      etms:          Map[ScorpioFilter, ExposureTimeModeId]
    ): AppliedFragment =
      Insert(
        observationId,
        input.fpu,
        etms(ScorpioFilter.G),
        etms(ScorpioFilter.R),
        etms(ScorpioFilter.I),
        etms(ScorpioFilter.Z),
        input.explicitSlitOffsetMode,
        input.formattedTelescopeConfigs,
        input.repeats.getOrElse(Config.DefaultRepeats)
      )

    def update(
      SET:   ScorpioLongSlitInput.Edit,
      which: NonEmptyList[Observation.Id]
    ): Option[AppliedFragment] =
      val upFpu              = sql"c_fpu               = $scorpio_fpu"
      val upSlitOffsetMode   = sql"c_slit_offset_mode  = ${slit_offset_mode.opt}"
      val upTelescopeConfigs = sql"c_telescope_configs = ${text.opt}"
      val upRepeats          = sql"c_repeats           = $int4_pos"

      NonEmptyList
        .fromList:
          List(
            SET.fpu.map(upFpu),
            SET.explicitSlitOffsetMode.toOptionOption.map(upSlitOffsetMode),
            SET.formattedTelescopeConfigs.toOptionOption.map(upTelescopeConfigs),
            SET.repeats.map(upRepeats)
          ).flatten
        .map: us =>
          void"UPDATE t_scorpio_long_slit " |+|
            void"SET " |+| us.intercalate(void", ") |+| void" " |+|
            void"WHERE " |+| observationIdIn(which)

    def updateEtm(
      which:   NonEmptyList[Observation.Id],
      channel: ScorpioFilter,
      update:  ExposureTimeMode
    ): AppliedFragment =
      sql"""
        UPDATE t_exposure_time_mode e
        SET c_exposure_time_mode = $exposure_time_mode_type,
            c_signal_to_noise    = ${signal_to_noise.opt},
            c_signal_to_noise_at = $wavelength_pm,
            c_exposure_time      = ${time_span.opt},
            c_exposure_count     = ${int4_pos.opt}
        FROM t_scorpio_long_slit s
        WHERE s.c_observation_id         = e.c_observation_id
          AND s.#${etmIdColumn(channel)} = e.c_exposure_time_mode_id
          AND """.apply(
        update.modeType,
        update.signalToNoise,
        update.at,
        update.exposureTime,
        update.frameCount
      ) |+| observationIdIn(which, "e".some)

    // Copies the exposure time and count, leaving the channel's own wavelength.
    def updateExposure(
      which:   NonEmptyList[Observation.Id],
      channel: ScorpioFilter,
      update:  ExposureTimeMode
    ): AppliedFragment =
      sql"""
        UPDATE t_exposure_time_mode e
        SET c_exposure_time  = ${time_span.opt},
            c_exposure_count = ${int4_pos.opt}
        FROM t_scorpio_long_slit s
        WHERE s.c_observation_id         = e.c_observation_id
          AND s.#${etmIdColumn(channel)} = e.c_exposure_time_mode_id
          AND """.apply(
        update.exposureTime,
        update.frameCount
      ) |+| observationIdIn(which, "e".some)

    def clone(
      originalId: Observation.Id,
      newId:      Observation.Id,
      etms:       List[(ExposureTimeModeId, ExposureTimeModeId)]
    ): AppliedFragment =
      val channels = ScorpioFilter.visible.toList

      val mappingValues: AppliedFragment =
        etms
          .map: (oldEtm, newEtm) =>
            sql"($exposure_time_mode_id, $exposure_time_mode_id)"(oldEtm, newEtm)
          .intercalate(void", ")

      def alias(channel: ScorpioFilter): String =
        s"${channel.tag.toLowerCase}_map"

      val joins: AppliedFragment =
        channels.foldMap: channel =>
          void" JOIN (VALUES " |+| mappingValues |+|
            sql") AS #${alias(channel)}(old_id, new_id) ON #${alias(channel)}.old_id = src.#${etmIdColumn(channel)}"(Void)

      sql"""
        INSERT INTO t_scorpio_long_slit (
          c_observation_id,
          c_program_id,
          c_fpu,
          #${channels.map(etmIdColumn).mkString(",\n")},
          c_slit_offset_mode,
          c_telescope_configs,
          c_repeats
        )
        SELECT
          $observation_id,
          src.c_program_id,
          src.c_fpu,
          #${channels.map(c => s"${alias(c)}.new_id").mkString(",\n")},
          src.c_slit_offset_mode,
          src.c_telescope_configs,
          src.c_repeats
        FROM t_scorpio_long_slit src"""(newId) |+|
        joins |+|
        sql" WHERE src.c_observation_id = $observation_id"(originalId)

    def delete(
      which: List[Observation.Id]
    ): Option[AppliedFragment] =
      NonEmptyList.fromList(which).map: oids =>
        void"DELETE FROM ONLY t_scorpio_long_slit WHERE " |+| observationIdIn(oids)
