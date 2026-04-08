// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.foldable.*
import cats.syntax.option.*
import grackle.Result
import grackle.ResultT
import lucuma.core.enums.GhostBinning
import lucuma.core.enums.GhostReadMode
import lucuma.core.enums.GhostResolutionMode
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.odb.data.ExposureTimeModeId
import lucuma.odb.graphql.input.GhostIfuInput
import lucuma.odb.sequence.ghost.ifu.Config
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GhostCodecs.*
import skunk.*
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
    newId:      Observation.Id
  ): F[Unit]

object GhostIfuService:
  enum Detector:
    case Red, Blue

  def instantiate[F[_]: {Concurrent, Services}]: GhostIfuService[F] =

    new GhostIfuService[F]:
      override def select(
        which:  List[Observation.Id]
      ): F[Map[Observation.Id, Config]] =
        ???

      override def insert(
        input:  GhostIfuInput.Create,
        reqEtm: Option[ExposureTimeMode],
        which:  List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        val science = NonEmptyList.of(
          Detector.Red  -> input.red.flatMap(_.exposureTimeMode),
          Detector.Blue -> input.blue.flatMap(_.exposureTimeMode)
        )

        NonEmptyList
          .fromList(which)
          .fold(ResultT.unit[F]): nel =>
            for
              r   <- ResultT(exposureTimeModeService.resolve("GHOST IFU", none, science, reqEtm, which))
              ids <- ResultT.liftF(exposureTimeModeService.insertResolvedAcquisitionAndScience(r))
              etms = nel.map: oid =>
                val m = ids(oid)._2.toList.toMap
                (oid, m(Detector.Red), m(Detector.Blue))
              _   <- ResultT.liftF(session.exec(Statements.insert(input, etms)))
            yield ()
          .value

      override def delete(
        which: List[Observation.Id]
      )(using Transaction[F]): F[Unit] =
        ???

      override def update(
        SET:   GhostIfuInput.Edit,
        which: List[Observation.Id]
      )(using Transaction[F]): F[Unit] =
        ???

      override def clone(
        originalId: Observation.Id,
        newId:      Observation.Id
      ): F[Unit] =
        ???

  object Statements:
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
              ${ghost_binning.opt},
              $ghost_binning,
              ${ghost_read_mode.opt},
              $ghost_read_mode,
              $exposure_time_mode_id,
              ${ghost_binning.opt},
              $ghost_binning,
              ${ghost_read_mode.opt},
              $ghost_read_mode,
              ${ghost_ifu1_fiber_agitator.opt},
              ${ghost_ifu2_fiber_agitator.opt}
            )"""(
              oid,
              input.resolutionMode,
              red,
              input.red.flatMap(_.explicitBinning.toOption),
              GhostBinning.OneByOne,
              input.red.flatMap(_.explicitReadMode.toOption),
              GhostReadMode.Medium,
              blue,
              input.blue.flatMap(_.explicitBinning.toOption),
              GhostBinning.OneByOne,
              input.blue.flatMap(_.explicitReadMode.toOption),
              GhostReadMode.Slow,
              input.explicitIfu1FiberAgitator,
              input.explicitIfu2FiberAgitator
            )

        void"""
          INSERT INTO t_ghost_ifu (
            c_observation_id,
            c_program_id,
            c_resolution_mode,
            c_red_exposure_time_mode_id,
            c_red_binning,
            c_red_binning_default,
            c_red_read_mode,
            c_red_read_mode_default,
            c_blue_exposure_time_mode_id,
            c_blue_binning,
            c_blue_binning_default,
            c_blue_read_mode,
            c_blue_read_mode_default,
            c_ifu1_fiber_agitator,
            c_ifu2_fiber_agitator
          )
          SELECT
            g.c_observation_id,
            o.c_program_id,
            g.c_resolution_mode,
            g.c_red_exposure_time_mode_id,
            g.c_red_binning,
            g.c_red_binning_default,
            g.c_red_read_mode,
            g.c_red_read_mode_default,
            g.c_blue_exposure_time_mode_id,
            g.c_blue_binning,
            g.c_blue_binning_default,
            g.c_blue_read_mode,
            g.c_blue_read_mode_default,
            g.c_ifu1_fiber_agitator,
            g.c_ifu2_fiber_agitator
          FROM (
            VALUES""" |+| modeEntries.intercalate(void", ") |+| void"""
          ) AS g (
            c_observation_id,
            c_resolution_mode,
            c_red_exposure_time_mode_id,
            c_red_binning,
            c_red_binning_default,
            c_red_read_mode,
            c_red_read_mode_default,
            c_blue_exposure_time_mode_id,
            c_blue_binning,
            c_blue_binning_default,
            c_blue_read_mode,
            c_blue_read_mode_default,
            c_ifu1_fiber_agitator,
            c_ifu2_fiber_agitator
          )
          JOIN t_observation o ON o.c_observation_id = g.c_observation_id
        """