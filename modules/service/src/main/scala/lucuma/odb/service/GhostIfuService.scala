// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.option.*
import grackle.Result
import grackle.ResultT
import lucuma.core.enums.GhostBinning
import lucuma.core.enums.GhostReadMode
import lucuma.core.enums.GhostResolutionMode
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.odb.data.ExposureTimeModeId
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.input.GhostIfuInput
import lucuma.odb.sequence.ghost.DetectorConfig
import lucuma.odb.sequence.ghost.ifu.Config
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GhostCodecs.*
import skunk.*
import skunk.data.Completion
import skunk.implicits.*

import Services.Syntax.*

trait GhostIfuService[F[_]]:
  def select(
    which: List[Observation.Id]
  ): F[Map[Observation.Id, Config]]

  def insert(
    input:  GhostIfuInput.Create,
    reqEtm: Option[ExposureTimeMode],
    which:  List[Observation.Id]
  )(using Transaction[F]): F[Result[Unit]]

  def delete(
    which: List[Observation.Id]
  )(using Transaction[F]): F[Unit]

  def update(
    SET:   GhostIfuInput.Edit,
    which: List[Observation.Id]
  )(using Transaction[F]): F[Unit]

  def clone(
    originalId: Observation.Id,
    newId:      Observation.Id,
    etms:       List[(ExposureTimeModeId, ExposureTimeModeId)]
  )(using Transaction[F]): F[Unit]

object GhostIfuService:
  enum Channel:
    case Red, Blue

  private def validateEtms(
    requirementsEtm: Option[ExposureTimeMode],
    scienceEtms:     Map[Channel, Option[ExposureTimeMode]]
  ): Result[Unit] =
    def validateEtm(c: Channel): Result[Unit] =
      Result
        .fromOption(
          scienceEtms
            .get(c)
            .flatten
            .orElse(requirementsEtm)
            .flatMap(ExposureTimeMode.timeAndCount.getOption)
            .void,
          OdbError.InvalidArgument("GHOST observations require a TimeAndCount exposure time mode.".some).asProblem
        )

    validateEtm(Channel.Red) *> validateEtm(Channel.Blue)

  def instantiate[F[_]: {Concurrent, Services}]: GhostIfuService[F] =

    new GhostIfuService[F]:
      override def select(
        which:  List[Observation.Id]
      ): F[Map[Observation.Id, Config]] =
        NonEmptyList
          .fromList(which)
          .fold(List.empty.pure[F]): oids =>
            val af = Statements.select(oids)
            session.prepareR(af.fragment.query(observation_id *: Statements.ghost_ifu)).use: pq =>
              pq.stream(af.argument, chunkSize = 1024).compile.toList
          .map(_.toMap)

      override def insert(
        input:  GhostIfuInput.Create,
        reqEtm: Option[ExposureTimeMode],
        which:  List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        val science = NonEmptyList.of(
          Channel.Blue -> input.blue.flatMap(_.exposureTimeMode),
          Channel.Red  -> input.red.flatMap(_.exposureTimeMode)
        )

        NonEmptyList
          .fromList(which)
          .fold(ResultT.unit[F]): nel =>
            for
              _   <- ResultT.fromResult(validateEtms(reqEtm, science.toList.toMap))
              r   <- ResultT(exposureTimeModeService.resolve("GHOST IFU", none, science, reqEtm, which))
              ids <- ResultT.liftF(exposureTimeModeService.insertResolvedAcquisitionAndScience(r))
              etms = nel.map: oid =>
                val m = ids(oid)._2.toList.toMap
                (oid, m(Channel.Red), m(Channel.Blue))
              _   <- ResultT.liftF(session.exec(Statements.insert(input, etms)))
            yield ()
          .value

      override def delete(
        which: List[Observation.Id]
      )(using Transaction[F]): F[Unit] =
        Statements.delete(which).traverse_(session.exec)

      override def update(
        SET:   GhostIfuInput.Edit,
        which: List[Observation.Id]
      )(using Transaction[F]): F[Unit] =
        ???

      override def clone(
        originalId: Observation.Id,
        newId:      Observation.Id,
        etms:       List[(ExposureTimeModeId, ExposureTimeModeId)]
      )(using Transaction[F]): F[Unit] =
        session
          .executeCommand(Statements.clone(originalId, newId, etms))
          .flatMap:
            case Completion.Insert(1) =>
              ().pure[F]
            case _                    =>
              Concurrent[F].raiseError(new RuntimeException(s"Could not clone Ghost IFU observing mode $originalId, $newId"))

  object Statements:

    val ghost_detector: Decoder[DetectorConfig] =
      (
        exposure_time_mode  *:
        ghost_binning       *:
        ghost_binning.opt   *:
        ghost_read_mode     *:
        ghost_read_mode.opt
      ).emap: (etm, defaultBinning, explicitBinning, defaultReadMode, explicitReadMode) =>
        ExposureTimeMode
          .timeAndCount
          .getOption(etm)
          .toRight(s"GHOST only supports time and count exposure time mode.")
          .map: tc =>
            DetectorConfig(tc, defaultBinning, explicitBinning, defaultReadMode, explicitReadMode)

    val ghost_detector_blue: Decoder[DetectorConfig.Blue] =
      (ghost_detector).map(DetectorConfig.Blue(_))

    val ghost_detector_red: Decoder[DetectorConfig.Red] =
      (ghost_detector).map(DetectorConfig.Red(_))

    val ghost_ifu: Decoder[Config] =
      (
        ghost_resolution_mode         *:
        ghost_detector_red            *:
        ghost_detector_blue           *:
        ghost_ifu1_fiber_agitator.opt *:
        ghost_ifu2_fiber_agitator.opt
      ).to[Config]

    private def detectorColumns(color: String): List[String] =
      List(
        s"c_${color}_exposure_time_mode_id",
        s"c_${color}_binning_default",
        s"c_${color}_binning",
        s"c_${color}_read_mode_default",
        s"c_${color}_read_mode"
      )

    private val AgitatorColumns: List[String] =
      List(
        "c_ifu1_fiber_agitator",
        "c_ifu2_fiber_agitator"
      )

    private val Columns: List[String] =
      List(
        "c_observation_id",
        "c_resolution_mode"
      )                       ++
      detectorColumns("red")  ++
      detectorColumns("blue") ++
      AgitatorColumns

    private val ExposureTimeColumns: List[String] =
      List(
        "c_exposure_time_mode",
        "c_signal_to_noise_at",
        "c_signal_to_noise",
        "c_exposure_time",
        "c_exposure_count"
      )

    extension (cols: List[String])
      def prefixed(prefix: String): List[String] = cols.map(c => s"$prefix.$c")
      def string: String = cols.mkString("", ",\n", "\n")
      def insert(index: Int, s: String*): List[String] = cols.patch(index, s.toList, 0)

    def select(
      which: NonEmptyList[Observation.Id]
    ): AppliedFragment =
      sql"""
        SELECT
          g.c_observation_id,
          g.c_resolution_mode,
          #${ExposureTimeColumns.prefixed("red").string},
          #${detectorColumns("red").tail.prefixed("g").string},
          #${ExposureTimeColumns.prefixed("blue").string},
          #${detectorColumns("blue").tail.prefixed("g").string},
          #${AgitatorColumns.prefixed("g").string}
        FROM
          t_ghost_ifu g
        JOIN t_exposure_time_mode red  ON g.c_red_exposure_time_mode_id  = red.c_exposure_time_mode_id
        JOIN t_exposure_time_mode blue ON g.c_blue_exposure_time_mode_id = blue.c_exposure_time_mode_id
        WHERE
          g.c_observation_id IN ("""(Void)                       |+|
            which.map(sql"$observation_id").intercalate(void",") |+|
          void")"

    def insert(
      input: GhostIfuInput.Create,
      which: NonEmptyList[(Observation.Id, ExposureTimeModeId, ExposureTimeModeId)]
    ): AppliedFragment =
      val modeEntries =
        which.map: (oid, red, blue) =>
          sql"""(
            $observation_id,
            $ghost_resolution_mode,
            $exposure_time_mode_id,
            $ghost_binning,
            ${ghost_binning.opt},
            $ghost_read_mode,
            ${ghost_read_mode.opt},
            $exposure_time_mode_id,
            $ghost_binning,
            ${ghost_binning.opt},
            $ghost_read_mode,
            ${ghost_read_mode.opt},
            ${ghost_ifu1_fiber_agitator.opt},
            ${ghost_ifu2_fiber_agitator.opt}
          )"""(
            oid,
            input.resolutionMode,
            red,
            GhostBinning.OneByOne,
            input.red.flatMap(_.explicitBinning.toOption),
            GhostReadMode.Medium,
            input.red.flatMap(_.explicitReadMode.toOption),
            blue,
            GhostBinning.OneByOne,
            input.blue.flatMap(_.explicitBinning.toOption),
            GhostReadMode.Slow,
            input.blue.flatMap(_.explicitReadMode.toOption),
            input.explicitIfu1FiberAgitator,
            input.explicitIfu2FiberAgitator
          )

      sql"""
        INSERT INTO t_ghost_ifu (
          #${Columns.insert(1, "c_program_id").string}
        )
        SELECT
          #${Columns.prefixed("g").insert(1, "o.c_program_id").string}
        FROM (
          VALUES"""(Void) |+| modeEntries.intercalate(void", ") |+| sql"""
        ) AS g (
          #${Columns.string}
        )
        JOIN t_observation o ON o.c_observation_id = g.c_observation_id
      """(Void)

    def delete(
      which: List[Observation.Id]
    ): Option[AppliedFragment] =
      NonEmptyList.fromList(which).map: oids =>
        void"DELETE FROM ONLY t_ghost_ifu WHERE " |+| observationIdIn(oids)

    def clone(
      originalId: Observation.Id,
      newId:      Observation.Id,
      etms:       List[(ExposureTimeModeId, ExposureTimeModeId)]
    ): AppliedFragment =

      val replace = List(
        "c_observation_id",
        "c_red_exposure_time_mode_id",
        "c_blue_exposure_time_mode_id"
      )

      val others  = Columns.insert(1, "c_program_id").filterNot(replace.toSet)
      val all     = replace ++ others

      val mappingValues: AppliedFragment =
        etms
          .map: (oldEtm, newEtm) =>
            sql"($exposure_time_mode_id, $exposure_time_mode_id)"(oldEtm, newEtm)
          .intercalate(void", ")

      sql"""
        INSERT INTO t_ghost_ifu (
          #${all.string}
        )
        SELECT
          $observation_id,
          red_map.new_id,
          blue_map.new_id,
          #${others.prefixed("src").string}
        FROM t_ghost_ifu src
        JOIN (VALUES"""(newId) |+| mappingValues |+| void""") AS red_map(old_id, new_id)
          ON red_map.old_id = src.c_red_exposure_time_mode_id
        JOIN (VALUES""" |+| mappingValues |+| sql""") AS blue_map(old_id, new_id)
          ON blue_map.old_id = src.c_blue_exposure_time_mode_id
        WHERE src.c_observation_id = $observation_id"""(originalId)