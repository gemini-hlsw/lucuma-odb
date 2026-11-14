// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Concurrent
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
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.ScorpioCodecs.*
import skunk.*
import skunk.codec.text.text
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

object ScorpioLongSlitService:

  private type ChannelModes = NonEmptyList[(ScorpioFilter, ExposureTimeMode)]

  private object validateEtms:

    private def failure(msg: String): Result[Unit] =
      OdbError.InvalidArgument(msg.some).asFailure

    private def timeAndCount(modes: ChannelModes): Result[Unit] =
      failure(s"${ObservingMode.ScorpioLongSlitName} requires time and count exposure time modes.")
        .unlessA(modes.forall((_, etm) => ExposureTimeMode.timeAndCount.getOption(etm).isDefined))

    // Channels read out by one detector controller cannot be exposed differently.
    private def sharedController(modes: ChannelModes, a: ScorpioFilter, b: ScorpioFilter): Result[Unit] =
      def exposure(f: ScorpioFilter) =
        modes
          .collectFirst { case (`f`, etm) => etm }
          .flatMap(ExposureTimeMode.timeAndCount.getOption)
          .map(tc => (tc.time, tc.count.value))

      failure(s"The SCORPIO ${a.shortName} and ${b.shortName} channels share a detector controller and must have the same exposure time and count.")
        .unlessA(exposure(a) === exposure(b))

    def apply(modes: ChannelModes): Result[Unit] =
      timeAndCount(modes) *>
        sharedController(modes, ScorpioFilter.G, ScorpioFilter.R) *>
        sharedController(modes, ScorpioFilter.I, ScorpioFilter.Z)

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
              _   <- ResultT.fromResult(sci.values.toList.traverse_(validateEtms.apply))
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

  object Statements:

    val scorpio_long_slit: Decoder[Config] =
      (
        scorpio_fpu        *:
        exposure_time_mode *:
        exposure_time_mode *:
        exposure_time_mode *:
        exposure_time_mode *:
        slit_offset_mode   *:
        text
      ).emap: (fpu, g, r, i, z, offsetMode, configsText) =>
        SlitTelescopeConfigsFormat
          .getOption((offsetMode, configsText))
          .toRight(s"Could not parse '$configsText' as telescope configs (mode ${offsetMode.tag}).")
          .map(Config(fpu, g, r, i, z, _))

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
          ls.c_telescope_configs_effective
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
      Option[String]
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
          c_telescope_configs
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
          ${text.opt}
        FROM t_observation
        WHERE c_observation_id = $observation_id
      """.contramap { (o, f, g, r, i, z, m, tc) => (o, f, g, r, i, z, m, tc, o) }

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
        input.formattedTelescopeConfigs
      )

    def delete(
      which: List[Observation.Id]
    ): Option[AppliedFragment] =
      NonEmptyList.fromList(which).map: oids =>
        void"DELETE FROM ONLY t_scorpio_long_slit WHERE " |+| observationIdIn(oids)
