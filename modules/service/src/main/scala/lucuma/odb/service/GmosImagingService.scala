// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Applicative
import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.all.*
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.model.Observation
import lucuma.odb.graphql.input.GmosImagingInput
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GmosCodecs.*
import skunk.*
import skunk.implicits.*

import Services.Syntax.*

sealed trait GmosImagingService[F[_]] {

  def insertNorth(
    input: GmosImagingInput.Create.North
  )(which: List[Observation.Id])(using Transaction[F]): F[Unit]

  def insertSouth(
    input: GmosImagingInput.Create.South
  )(which: List[Observation.Id])(using Transaction[F]): F[Unit]

  def deleteNorth(
    which: List[Observation.Id]
  )(using Transaction[F]): F[Unit]

  def deleteSouth(
    which: List[Observation.Id]
  )(using Transaction[F]): F[Unit]

  def updateNorth(
    SET: GmosImagingInput.Edit.North
  )(which: List[Observation.Id])(using Transaction[F]): F[Unit]

  def updateSouth(
    SET: GmosImagingInput.Edit.South
  )(which: List[Observation.Id])(using Transaction[F]): F[Unit]
  def cloneNorth(
    observationId: Observation.Id,
    newObservationId: Observation.Id
  )(using Transaction[F]): F[Unit]

  def cloneSouth(
    observationId: Observation.Id,
    newObservationId: Observation.Id
  )(using Transaction[F]): F[Unit]
}

object GmosImagingService {

  def instantiate[F[_]: Concurrent](using Services[F]): GmosImagingService[F] =
    new GmosImagingService[F] {

      override def insertNorth(
        input: GmosImagingInput.Create.North
      )(which: List[Observation.Id])(using Transaction[F]): F[Unit] =
        session.exec(Statements.insertGmosNorthImaging(which, input))

      override def insertSouth(
        input: GmosImagingInput.Create.South
      )(which: List[Observation.Id])(using Transaction[F]): F[Unit] =
        session.exec(Statements.insertGmosSouthImaging(which, input))

      override def deleteNorth(
        which: List[Observation.Id]
      )(using Transaction[F]): F[Unit] =
        which.traverse_ { oid =>
          session.exec(Statements.deleteGmosNorthImagingMode(oid)) *>
          session.exec(Statements.deleteGmosNorthImagingFilters(oid))
        }

      override def deleteSouth(
        which: List[Observation.Id]
      )(using Transaction[F]): F[Unit] =
        which.traverse_ { oid =>
          session.exec(Statements.deleteGmosSouthImagingMode(oid)) *>
          session.exec(Statements.deleteGmosSouthImagingFilters(oid))
        }

      override def updateNorth(
        SET: GmosImagingInput.Edit.North
      )(which: List[Observation.Id])(using Transaction[F]): F[Unit] =
        val commonUpdates = Statements.updateGmosNorthImaging(SET, which).fold(Applicative[F].unit)(session.exec)
        val filterUpdates = SET.filters match {
          case Some(filters) =>
            which.traverse_ { oid =>
              session.exec(Statements.deleteGmosNorthImagingFilters(oid)) *>
              (if (filters.nonEmpty)
                 session.exec(Statements.insertGmosNorthImagingFiltersForUpdate(oid, filters))
               else
                 Concurrent[F].unit)
            }
          case None => Applicative[F].unit
        }
        commonUpdates *> filterUpdates

      override def updateSouth(
        SET: GmosImagingInput.Edit.South
      )(which: List[Observation.Id])(using Transaction[F]): F[Unit] =
        val commonUpdates = Statements.updateGmosSouthImaging(SET, which).fold(Applicative[F].unit)(session.exec)
        val filterUpdates = SET.filters match {
          case Some(filters) =>
            which.traverse_ { oid =>
              session.exec(Statements.deleteGmosSouthImagingFilters(oid)) *>
              (if (filters.nonEmpty)
                 session.exec(Statements.insertGmosSouthImagingFiltersForUpdate(oid, filters))
               else
                 Concurrent[F].unit)
            }
          case None => Applicative[F].unit
        }
        commonUpdates *> filterUpdates

      override def cloneNorth(
        observationId: Observation.Id,
        newObservationId: Observation.Id
      )(using Transaction[F]): F[Unit] =
        session.exec(Statements.cloneGmosNorthImagingMode(observationId, newObservationId)) *>
        session.exec(Statements.cloneGmosNorthImagingFilters(observationId, newObservationId))

      override def cloneSouth(
        observationId: Observation.Id,
        newObservationId: Observation.Id
      )(using Transaction[F]): F[Unit] =
        session.exec(Statements.cloneGmosSouthImagingMode(observationId, newObservationId)) *>
        session.exec(Statements.cloneGmosSouthImagingFilters(observationId, newObservationId))
    }

  object Statements {

    // Generic helper for combined insert statements
    private def insertGmosImaging(
      modeTableName: String,
      filterTableName: String,
      oids: List[Observation.Id],
      common: GmosImagingInput.Create.Common,
      filterEntries: List[AppliedFragment]
    ): AppliedFragment = {
      val modeEntries =
        oids.map { oid =>
          sql"""(
            $observation_id,
            ${gmos_binning.opt},
            ${gmos_amp_read_mode.opt},
            ${gmos_amp_gain.opt},
            ${gmos_roi.opt}
          )"""(
            oid,
            common.explicitBin,
            common.explicitAmpReadMode,
            common.explicitAmpGain,
            common.explicitRoi
          )
        }

      val modeValues: AppliedFragment = modeEntries.intercalate(void", ")
      
      if (filterEntries.nonEmpty) {
        val filterValues: AppliedFragment = filterEntries.intercalate(void", ")
        sql"""
          WITH mode_inserts AS (
            INSERT INTO #$modeTableName (
              c_observation_id,
              c_explicit_bin,
              c_explicit_amp_read_mode,
              c_explicit_amp_gain,
              c_explicit_roi
            ) VALUES """.apply(Void) |+| modeValues |+| sql"""
            RETURNING c_observation_id
          )
          INSERT INTO #$filterTableName (
            c_observation_id,
            c_filter
          ) VALUES """.apply(Void) |+| filterValues
      } else {
        sql"INSERT INTO #$modeTableName (c_observation_id, c_explicit_bin, c_explicit_amp_read_mode, c_explicit_amp_gain, c_explicit_roi) VALUES ".apply(Void) |+| modeValues
      }
    }

    // Combined insert statements - inserts both mode and filters in a single call
    def insertGmosNorthImaging(
      oids: List[Observation.Id],
      input: GmosImagingInput.Create.North
    ): AppliedFragment = {
      val filterEntries = for {
        oid <- oids
        filter <- input.filters.toList
      } yield sql"""($observation_id, $gmos_north_filter)"""(oid, filter)

      insertGmosImaging(
        "t_gmos_north_imaging",
        "t_gmos_north_imaging_filter",
        oids,
        input.common,
        filterEntries
      )
    }

    def insertGmosSouthImaging(
      oids: List[Observation.Id],
      input: GmosImagingInput.Create.South
    ): AppliedFragment = {
      val filterEntries = for {
        oid <- oids
        filter <- input.filters.toList
      } yield sql"""($observation_id, $gmos_south_filter)"""(oid, filter)

      insertGmosImaging(
        "t_gmos_south_imaging",
        "t_gmos_south_imaging_filter",
        oids,
        input.common,
        filterEntries
      )
    }

    // Delete statements
    def deleteGmosNorthImagingMode(oid: Observation.Id): AppliedFragment =
      sql"""
        DELETE FROM t_gmos_north_imaging
        WHERE c_observation_id = $observation_id
      """.apply(oid)

    def deleteGmosNorthImagingFilters(oid: Observation.Id): AppliedFragment =
      sql"""
        DELETE FROM t_gmos_north_imaging_filter
        WHERE c_observation_id = $observation_id
      """.apply(oid)

    def deleteGmosSouthImagingMode(oid: Observation.Id): AppliedFragment =
      sql"""
        DELETE FROM t_gmos_south_imaging
        WHERE c_observation_id = $observation_id
      """.apply(oid)

    def deleteGmosSouthImagingFilters(oid: Observation.Id): AppliedFragment =
      sql"""
        DELETE FROM t_gmos_south_imaging_filter
        WHERE c_observation_id = $observation_id
      """.apply(oid)
    // Clone statements
    def cloneGmosNorthImagingMode(
      originalId: Observation.Id,
      newId: Observation.Id
    ): AppliedFragment =
      sql"""
        INSERT INTO t_gmos_north_imaging (
          c_observation_id,
          c_explicit_bin,
          c_explicit_amp_read_mode,
          c_explicit_amp_gain,
          c_explicit_roi
        )
        SELECT
          $observation_id,
          c_explicit_bin,
          c_explicit_amp_read_mode,
          c_explicit_amp_gain,
          c_explicit_roi
        FROM t_gmos_north_imaging
        WHERE c_observation_id = $observation_id
      """.apply(newId, originalId)

    def cloneGmosNorthImagingFilters(
      originalId: Observation.Id,
      newId: Observation.Id
    ): AppliedFragment =
      sql"""
        INSERT INTO t_gmos_north_imaging_filter (
          c_observation_id,
          c_filter
        )
        SELECT
          $observation_id,
          c_filter
        FROM t_gmos_north_imaging_filter
        WHERE c_observation_id = $observation_id
      """.apply(newId, originalId)

    def cloneGmosSouthImagingMode(
      originalId: Observation.Id,
      newId: Observation.Id
    ): AppliedFragment =
      sql"""
        INSERT INTO t_gmos_south_imaging (
          c_observation_id,
          c_explicit_bin,
          c_explicit_amp_read_mode,
          c_explicit_amp_gain,
          c_explicit_roi
        )
        SELECT
          $observation_id,
          c_explicit_bin,
          c_explicit_amp_read_mode,
          c_explicit_amp_gain,
          c_explicit_roi
        FROM t_gmos_south_imaging
        WHERE c_observation_id = $observation_id
      """.apply(newId, originalId)

    def cloneGmosSouthImagingFilters(
      originalId: Observation.Id,
      newId: Observation.Id
    ): AppliedFragment =
      sql"""
        INSERT INTO t_gmos_south_imaging_filter (
          c_observation_id,
          c_filter
        )
        SELECT
          $observation_id,
          c_filter
        FROM t_gmos_south_imaging_filter
        WHERE c_observation_id = $observation_id
      """.apply(newId, originalId)

    // Update statements following the GmosLongSlitService pattern
    def commonUpdates(
      input: GmosImagingInput.Edit.Common
    ): List[AppliedFragment] = {
      val upBin = sql"c_explicit_bin = ${gmos_binning.opt}"
      val upAmpReadMode = sql"c_explicit_amp_read_mode = ${gmos_amp_read_mode.opt}"
      val upAmpGain = sql"c_explicit_amp_gain = ${gmos_amp_gain.opt}"
      val upRoi = sql"c_explicit_roi = ${gmos_roi.opt}"

      List(
        input.explicitBin.toOptionOption.map(upBin),
        input.explicitAmpReadMode.toOptionOption.map(upAmpReadMode),
        input.explicitAmpGain.toOptionOption.map(upAmpGain),
        input.explicitRoi.toOptionOption.map(upRoi)
      ).flatten
    }

    def gmosNorthImagingUpdates(
      input: GmosImagingInput.Edit.North
    ): Option[NonEmptyList[AppliedFragment]] = {
      val ups: List[AppliedFragment] = commonUpdates(input.common)
      NonEmptyList.fromList(ups)
    }

    def updateGmosNorthImaging(
      SET: GmosImagingInput.Edit.North,
      which: List[Observation.Id]
    ): Option[AppliedFragment] =
      for {
        us <- gmosNorthImagingUpdates(SET)
        oids <- NonEmptyList.fromList(which)
      } yield
        void"UPDATE t_gmos_north_imaging " |+|
          void"SET " |+| us.intercalate(void", ") |+| void" " |+|
          void"WHERE " |+| observationIdIn(oids)

    def gmosSouthImagingUpdates(
      input: GmosImagingInput.Edit.South
    ): Option[NonEmptyList[AppliedFragment]] = {
      val ups: List[AppliedFragment] = commonUpdates(input.common)
      NonEmptyList.fromList(ups)
    }

    def updateGmosSouthImaging(
      SET: GmosImagingInput.Edit.South,
      which: List[Observation.Id]
    ): Option[AppliedFragment] =
      for {
        us <- gmosSouthImagingUpdates(SET)
        oids <- NonEmptyList.fromList(which)
      } yield
        void"UPDATE t_gmos_south_imaging " |+|
          void"SET " |+| us.intercalate(void", ") |+| void" " |+|
          void"WHERE " |+| observationIdIn(oids)

    // Helper methods for updating single observation filters
    def insertGmosNorthImagingFiltersForUpdate(
      oid: Observation.Id,
      filters: NonEmptyList[GmosNorthFilter]
    ): AppliedFragment = {
      def insertFilters: AppliedFragment =
        void"""
          INSERT INTO t_gmos_north_imaging_filter (
            c_observation_id,
            c_filter
          ) VALUES
        """

      def filterEntries =
        filters.map { filter =>
          sql"""($observation_id, $gmos_north_filter)"""(oid, filter)
        }

      val filterValues: AppliedFragment = filterEntries.intercalate(void", ")
      insertFilters |+| filterValues
    }

    def insertGmosSouthImagingFiltersForUpdate(
      oid: Observation.Id,
      filters: NonEmptyList[GmosSouthFilter]
    ): AppliedFragment = {
      def insertFilters: AppliedFragment =
        void"""
          INSERT INTO t_gmos_south_imaging_filter (
            c_observation_id,
            c_filter
          ) VALUES
        """

      def filterEntries =
        filters.map { filter =>
          sql"""($observation_id, $gmos_south_filter)"""(oid, filter)
        }

      val filterValues: AppliedFragment = filterEntries.intercalate(void", ")
      insertFilters |+| filterValues
    }

  }

}
