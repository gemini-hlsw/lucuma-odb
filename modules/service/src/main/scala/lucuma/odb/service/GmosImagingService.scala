// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Concurrent
import cats.syntax.all.*
import grackle.Result
import grackle.syntax.*
import lucuma.core.enums.{GmosAmpGain, GmosAmpReadMode, GmosNorthFilter, GmosRoi, GmosSouthFilter, GmosXBinning, GmosYBinning, ObservingModeType}
import lucuma.core.model.{Observation, SourceProfile}
import lucuma.core.model.sequence.gmos.{DynamicConfig, GmosCcdMode}
import lucuma.odb.data.{OdbError, OdbErrorExtensions}
import lucuma.odb.graphql.input.GmosImagingInput
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GmosCodecs.*
import skunk.*
import skunk.codec.all.*
import skunk.implicits.*

import Services.Syntax.*

sealed trait GmosImagingService[F[_]] {

  // def selectNorth(
  //   which: List[Observation.Id]
  // )(using Transaction[F]): F[Map[Observation.Id, SourceProfile => DynamicConfig.GmosNorth]]
  //
  // def selectSouth(
  //   which: List[Observation.Id]
  // )(using Transaction[F]): F[Map[Observation.Id, SourceProfile => DynamicConfig.GmosSouth]]
  //
  def insertNorth(
    input: GmosImagingInput.Create.North
  )(which: List[Observation.Id])(using Transaction[F]): F[Unit]

  def insertSouth(
    input: GmosImagingInput.Create.South
  )(which: List[Observation.Id])(using Transaction[F]): F[Unit]

  // def deleteNorth(
  //   which: List[Observation.Id]
  // )(using Transaction[F]): F[Unit]
  //
  // def deleteSouth(
  //   which: List[Observation.Id]
  // )(using Transaction[F]): F[Unit]
  //
  // def updateNorth(
  //   input: GmosImagingInput.Edit.North
  // )(which: List[Observation.Id])(using Transaction[F]): F[Result[Unit]]
  //
  // def updateSouth(
  //   input: GmosImagingInput.Edit.South
  // )(which: List[Observation.Id])(using Transaction[F]): F[Result[Unit]]
  //
  // def cloneNorth(
  //   observationId: Observation.Id,
  //   newObservationId: Observation.Id
  // )(using Transaction[F]): F[Unit]
  //
  // def cloneSouth(
  //   observationId: Observation.Id,
  //   newObservationId: Observation.Id
  // )(using Transaction[F]): F[Unit]
  //
}

object GmosImagingService {

  def instantiate[F[_]: Concurrent](using Services[F]): GmosImagingService[F] =
    new GmosImagingService[F] {

      // object DecodersNorth {
      //
      //   val decoder: Decoder[SourceProfile => DynamicConfig.GmosNorth] =
      //     (
      //       _gmos_north_filter                *:
      //        gmos_binning.opt                  *:
      //        gmos_amp_read_mode.opt            *:
      //        gmos_amp_gain.opt                 *:
      //        gmos_roi.opt                      *:
      //        gmos_binning                      *:
      //        gmos_amp_read_mode                *:
      //        gmos_amp_gain                     *:
      //        gmos_roi
      //     ).emap { case ((((((((filters, explicitBin), explicitAmpReadMode), explicitAmpGain), explicitRoi), bin), ampReadMode), ampGain), roi) =>
      //       (_: SourceProfile) =>
      //         val ccdMode = GmosCcdMode(
      //           xBin    = GmosXBinning(bin),
      //           yBin    = GmosYBinning(bin),
      //           ampCount = if (filters.length > 3) gmos.GmosAmpCount.Twelve else gmos.GmosAmpCount.Six, // TODO: Calculate amp count based on requirements
      //           ampGain = ampGain,
      //           ampReadMode = ampReadMode
      //         )
      //
      //         DynamicConfig.GmosNorth(
      //           exposure   = core.util.TimeSpan.Zero, // TODO: Calculate from exposure time mode
      //           readout    = ccdMode,
      //           dtax       = gmos.GmosDtax.Zero,
      //           roi        = roi,
      //           gratingConfig = None, // No grating for imaging
      //           filter     = filters.headOption, // Use first filter for now - TODO: Handle multiple filters
      //           fpu        = None  // No FPU for imaging
      //         )
      //     }
      //
      // }
      //
      // object DecodersSouth {
      //
      //   val decoder: Decoder[SourceProfile => DynamicConfig.GmosSouth] =
      //     (
      //       (_gmos_south_filter                *:
      //        gmos_binning.opt                  *:
      //        gmos_amp_read_mode.opt            *:
      //        gmos_amp_gain.opt                 *:
      //        gmos_roi.opt                      *:
      //        gmos_binning                      *:
      //        gmos_amp_read_mode                *:
      //        gmos_amp_gain                     *:
      //        gmos_roi).tupled
      //     ).map { case (filters, explicitBin, explicitAmpReadMode, explicitAmpGain, explicitRoi, bin, ampReadMode, ampGain, roi) =>
      //       (_: SourceProfile) =>
      //         val ccdMode = GmosCcdMode(
      //           xBin    = GmosXBinning(bin),
      //           yBin    = GmosYBinning(bin),
      //           ampCount = if (filters.length > 3) gmos.GmosAmpCount.Twelve else gmos.GmosAmpCount.Six, // TODO: Calculate amp count based on requirements
      //           ampGain = ampGain,
      //           ampReadMode = ampReadMode
      //         )
      //
      //         DynamicConfig.GmosSouth(
      //           exposure   = core.util.TimeSpan.Zero, // TODO: Calculate from exposure time mode
      //           readout    = ccdMode,
      //           dtax       = gmos.GmosDtax.Zero,
      //           roi        = roi,
      //           gratingConfig = None, // No grating for imaging
      //           filter     = filters.headOption, // Use first filter for now - TODO: Handle multiple filters
      //           fpu        = None  // No FPU for imaging
      //         )
      //     }
      //
      // }

      // override def selectNorth(
      //   which: List[Observation.Id]
      // )(using Transaction[F]): F[Map[Observation.Id, SourceProfile => DynamicConfig.GmosNorth]] =
      //   session.prepareR(selectGmosNorthImaging).use { pq =>
      //     pq.stream(which, 1024)
      //       .compile
      //       .toList
      //       .map(_.toMap)
      //   }
      //
      // override def selectSouth(
      //   which: List[Observation.Id]
      // )(using Transaction[F]): F[Map[Observation.Id, SourceProfile => DynamicConfig.GmosSouth]] =
      //   session.prepareR(selectGmosSouthImaging).use { pq =>
      //     pq.stream(which, 1024)
      //       .compile
      //       .toList
      //       .map(_.toMap)
      //   }
      //
      override def insertNorth(
        input: GmosImagingInput.Create.North
      )(which: List[Observation.Id])(using Transaction[F]): F[Unit] =
        session.exec(Statements.insertGmosNorthImaging(which, input)).void

      override def insertSouth(
        input: GmosImagingInput.Create.South
      )(which: List[Observation.Id])(using Transaction[F]): F[Unit] =
        session.exec(Statements.insertGmosSouthImaging(which, input)).void

      // override def deleteNorth(
      //   which: List[Observation.Id]
      // )(using Transaction[F]): F[Unit] =
      //   which.traverse_ { oid =>
      //     session.executeCommand(deleteGmosNorthImaging(oid))
      //   }
      //
      // override def deleteSouth(
      //   which: List[Observation.Id]
      // )(using Transaction[F]): F[Unit] =
      //   which.traverse_ { oid =>
      //     session.executeCommand(deleteGmosSouthImaging(oid))
      //   }
      //
      // override def updateNorth(
      //   input: GmosImagingInput.Edit.North
      // )(which: List[Observation.Id])(using Transaction[F]): F[Result[Unit]] =
      //   input.toCreate.traverse { createInput =>
      //     which.traverse_ { oid =>
      //       deleteNorth(List(oid)) *> insertNorth(createInput)(List(oid)).void
      //     }
      //   }
      //
      // override def updateSouth(
      //   input: GmosImagingInput.Edit.South
      // )(which: List[Observation.Id])(using Transaction[F]): F[Result[Unit]] =
      //   input.toCreate.traverse { createInput =>
      //     which.traverse_ { oid =>
      //       deleteSouth(List(oid)) *> insertSouth(createInput)(List(oid)).void
      //     }
      //   }
      //
      // override def cloneNorth(
      //   observationId: Observation.Id,
      //   newObservationId: Observation.Id
      // )(using Transaction[F]): F[Unit] =
      //   session.executeCommand(cloneGmosNorthImaging(observationId, newObservationId))
      //
      // override def cloneSouth(
      //   observationId: Observation.Id,
      //   newObservationId: Observation.Id
      // )(using Transaction[F]): F[Unit] =
      //   session.executeCommand(cloneGmosSouthImaging(observationId, newObservationId))
      //
    }

  object Statements {

    // Select statements using the views
    // val selectGmosNorthImaging: Query[List[Observation.Id], (Observation.Id, SourceProfile => DynamicConfig.GmosNorth)] =
    //   sql"""
    //     SELECT c_observation_id,
    //            c_filters,
    //            c_explicit_x_bin,
    //            c_explicit_y_bin,
    //            c_explicit_amp_read_mode,
    //            c_explicit_amp_gain,
    //            c_explicit_roi,
    //            c_x_bin,
    //            c_y_bin,
    //            c_amp_read_mode,
    //            c_amp_gain,
    //            c_roi
    //     FROM v_gmos_north_imaging
    //     WHERE c_observation_id = ANY($observation_id_array)
    //   """.query(observation_id *: DecodersNorth.decoder)
    //
    // val selectGmosSouthImaging: Query[List[Observation.Id], (Observation.Id, SourceProfile => DynamicConfig.GmosSouth)] =
    //   sql"""
    //     SELECT c_observation_id,
    //            c_filters,
    //            c_explicit_x_bin,
    //            c_explicit_y_bin,
    //            c_explicit_amp_read_mode,
    //            c_explicit_amp_gain,
    //            c_explicit_roi,
    //            c_x_bin,
    //            c_y_bin,
    //            c_amp_read_mode,
    //            c_amp_gain,
    //            c_roi
    //     FROM v_gmos_south_imaging
    //     WHERE c_observation_id = ANY($observation_id_array)
    //   """.query(observation_id *: DecodersSouth.decoder)

    // Insert statements following the array pattern
    def insertGmosNorthImaging(
      oids: List[Observation.Id],
      input: GmosImagingInput.Create.North,
    ): AppliedFragment = {
      def insertMode: AppliedFragment =
        void"""
          INSERT INTO t_gmos_north_imaging (
            c_observation_id,
            c_explicit_bin,
            c_explicit_amp_read_mode,
            c_explicit_amp_gain,
            c_explicit_roi
          ) VALUES
        """

      def modeEntries =
        oids.map { oid =>
          sql"""(
            $observation_id,
            ${gmos_binning.opt},
            ${gmos_amp_read_mode.opt},
            ${gmos_amp_gain.opt},
            ${gmos_roi.opt}
          )"""(
            oid,
            input.common.explicitBin,
            input.common.explicitAmpReadMode,
            input.common.explicitAmpGain,
            input.common.explicitRoi
          )
        }

      def insertFilters: AppliedFragment =
        void"""
          INSERT INTO t_gmos_north_imaging_filter (
            c_observation_id,
            c_filter
          ) VALUES
        """

      def filterEntries =
        for {
          oid <- oids
          filter <- input.filters
        } yield sql"""($observation_id, $gmos_north_filter)"""(oid, filter)

      val modeValues: AppliedFragment = modeEntries.intercalate(void", ")
      val filterValues: AppliedFragment = filterEntries.intercalate(void", ")

      if (input.filters.nonEmpty) {
        insertMode |+| modeValues |+| void"; " |+| insertFilters |+| filterValues
      } else {
        insertMode |+| modeValues
      }
    }

    def insertGmosSouthImaging(
      oids: List[Observation.Id],
      input: GmosImagingInput.Create.South
    ): AppliedFragment = {
      def insertMode: AppliedFragment =
        void"""
          INSERT INTO t_gmos_south_imaging (
            c_observation_id,
            c_explicit_bin,
            c_explicit_amp_read_mode,
            c_explicit_amp_gain,
            c_explicit_roi
          ) VALUES
        """

      def modeEntries =
        oids.map { oid =>
          sql"""(
            $observation_id,
            ${gmos_binning.opt},
            ${gmos_amp_read_mode.opt},
            ${gmos_amp_gain.opt},
            ${gmos_roi.opt}
          )"""(
            oid,
            input.common.explicitBin,
            input.common.explicitAmpReadMode,
            input.common.explicitAmpGain,
            input.common.explicitRoi
          )
        }

      def insertFilters: AppliedFragment =
        void"""
          INSERT INTO t_gmos_south_imaging_filter (
            c_observation_id,
            c_filter
          ) VALUES
        """

      def filterEntries =
        for {
          oid <- oids
          filter <- input.filters
        } yield sql"""($observation_id, $gmos_south_filter)"""(oid, filter)

      val modeValues: AppliedFragment = modeEntries.intercalate(void", ")
      val filterValues: AppliedFragment = filterEntries.intercalate(void", ")

      if (input.filters.nonEmpty) {
        insertMode |+| modeValues |+| void"; " |+| insertFilters |+| filterValues
      } else {
        insertMode |+| modeValues
      }
    }

    // Delete statements
    // def deleteGmosNorthImaging(oid: Observation.Id): Command[Observation.Id] =
    //   sql"""
    //     DELETE FROM t_gmos_north_imaging
    //     WHERE c_observation_id = $observation_id
    //   """.command
    //
    // def deleteGmosSouthImaging(oid: Observation.Id): Command[Observation.Id] =
    //   sql"""
    //     DELETE FROM t_gmos_south_imaging
    //     WHERE c_observation_id = $observation_id
    //   """.command
    //
    // // Clone statements
    // def cloneGmosNorthImaging(
    //   originalId: Observation.Id,
    //   newId: Observation.Id
    // ): Command[(Observation.Id, Observation.Id)] =
    //   sql"""
    //     INSERT INTO t_gmos_north_imaging (
    //       c_observation_id,
    //       c_observing_mode_type,
    //       c_explicit_x_bin,
    //       c_explicit_y_bin,
    //       c_explicit_amp_read_mode,
    //       c_explicit_amp_gain,
    //       c_explicit_roi
    //     )
    //     SELECT
    //       ${observation_id},
    //       c_observing_mode_type,
    //       c_explicit_x_bin,
    //       c_explicit_y_bin,
    //       c_explicit_amp_read_mode,
    //       c_explicit_amp_gain,
    //       c_explicit_roi
    //     FROM t_gmos_north_imaging
    //     WHERE c_observation_id = ${observation_id};
    //
    //     INSERT INTO t_gmos_north_imaging_filter (
    //       c_observation_id,
    //       c_filter
    //     )
    //     SELECT
    //       ${observation_id},
    //       c_filter
    //     FROM t_gmos_north_imaging_filter
    //     WHERE c_observation_id = ${observation_id}
    //   """.command.contramap { case (newId, originalId) => (newId, originalId, newId, originalId) }
    //
    // def cloneGmosSouthImaging(
    //   originalId: Observation.Id,
    //   newId: Observation.Id
    // ): Command[(Observation.Id, Observation.Id)] =
    //   sql"""
    //     INSERT INTO t_gmos_south_imaging (
    //       c_observation_id,
    //       c_observing_mode_type,
    //       c_explicit_x_bin,
    //       c_explicit_y_bin,
    //       c_explicit_amp_read_mode,
    //       c_explicit_amp_gain,
    //       c_explicit_roi
    //     )
    //     SELECT
    //       ${observation_id},
    //       c_observing_mode_type,
    //       c_explicit_x_bin,
    //       c_explicit_y_bin,
    //       c_explicit_amp_read_mode,
    //       c_explicit_amp_gain,
    //       c_explicit_roi
    //     FROM t_gmos_south_imaging
    //     WHERE c_observation_id = ${observation_id};
    //
    //     INSERT INTO t_gmos_south_imaging_filter (
    //       c_observation_id,
    //       c_filter
    //     )
    //     SELECT
    //       ${observation_id},
    //       c_filter
    //     FROM t_gmos_south_imaging_filter
    //     WHERE c_observation_id = ${observation_id}
    //   """.command.contramap { case (newId, originalId) => (newId, originalId, newId, originalId) }

  }

}
