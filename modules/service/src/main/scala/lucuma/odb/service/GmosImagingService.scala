// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.effect.MonadCancelThrow
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
        Statements.deleteGmosImaging("t_gmos_north_imaging", "t_gmos_north_imaging_filter", which)

      override def deleteSouth(
        which: List[Observation.Id]
      )(using Transaction[F]): F[Unit] =
        Statements.deleteGmosImaging("t_gmos_south_imaging", "t_gmos_south_imaging_filter", which)

      override def updateNorth(
        SET: GmosImagingInput.Edit.North
      )(which: List[Observation.Id])(using Transaction[F]): F[Unit] =
        Statements.updateGmosImaging(
          "t_gmos_north_imaging",
          "t_gmos_north_imaging_filter",
          SET.common,
          SET.filters,
          which,
          Statements.insertGmosNorthImagingFiltersForUpdate
        )

      override def updateSouth(
        SET: GmosImagingInput.Edit.South
      )(which: List[Observation.Id])(using Transaction[F]): F[Unit] =
        Statements.updateGmosImaging(
          "t_gmos_south_imaging",
          "t_gmos_south_imaging_filter",
          SET.common,
          SET.filters,
          which,
          Statements.insertGmosSouthImagingFiltersForUpdate
        )

      override def cloneNorth(
        observationId: Observation.Id,
        newObservationId: Observation.Id
      )(using Transaction[F]): F[Unit] =
        Statements.cloneGmosImaging("t_gmos_north_imaging", "t_gmos_north_imaging_filter", observationId, newObservationId)

      override def cloneSouth(
        observationId: Observation.Id,
        newObservationId: Observation.Id
      )(using Transaction[F]): F[Unit] =
        Statements.cloneGmosImaging("t_gmos_south_imaging", "t_gmos_south_imaging_filter", observationId, newObservationId)
    }

  object Statements {

    private def insertGmosImaging(
      modeTableName:   String,
      filterTableName: String,
      oids:            List[Observation.Id],
      common:          GmosImagingInput.Create.Common,
      filterEntries:   List[AppliedFragment]
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
            ) VALUES """(Void) |+| modeValues |+| sql"""
            RETURNING c_observation_id
          )
          INSERT INTO #$filterTableName (
            c_observation_id,
            c_filter
          ) VALUES """(Void) |+| filterValues
      } else {
        sql"INSERT INTO #$modeTableName (c_observation_id, c_explicit_bin, c_explicit_amp_read_mode, c_explicit_amp_gain, c_explicit_roi) VALUES "(Void) |+| modeValues
      }
    }

    // inserts both mode and filters in a single call
    def insertGmosNorthImaging(
      oids: List[Observation.Id],
      input: GmosImagingInput.Create.North
    ): AppliedFragment =
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
    def deleteGmosImaging[F[_]: MonadCancelThrow](
      modeTableName: String,
      filterTableName: String,
      which: List[Observation.Id]
    )(using Services[F]): F[Unit] =
      which.traverse_ { oid =>
        session.exec(Statements.deleteGmosImagingMode(modeTableName, oid)) *>
        session.exec(Statements.deleteGmosImagingFilters(filterTableName, oid))
      }

    private def deleteGmosImagingMode(tableName: String, oid: Observation.Id): AppliedFragment =
      sql"DELETE FROM #$tableName WHERE c_observation_id = ".apply(Void) |+| sql"$observation_id".apply(oid)

    def deleteGmosImagingFilters(tableName: String, oid: Observation.Id): AppliedFragment =
      sql"DELETE FROM #$tableName WHERE c_observation_id = ".apply(Void) |+| sql"$observation_id".apply(oid)

    def deleteGmosNorthImagingFilters(oid: Observation.Id): AppliedFragment =
      deleteGmosImagingFilters("t_gmos_north_imaging_filter", oid)

    def deleteGmosSouthImagingFilters(oid: Observation.Id): AppliedFragment =
      deleteGmosImagingFilters("t_gmos_south_imaging_filter", oid)

    // Clone statements
    def cloneGmosImaging[F[_]: MonadCancelThrow](
      modeTableName: String,
      filterTableName: String,
      observationId: Observation.Id,
      newObservationId: Observation.Id
    )(using Services[F]): F[Unit] =
      session.exec(Statements.cloneGmosImagingMode(modeTableName, observationId, newObservationId)) *>
        session.exec(Statements.cloneGmosImagingFilters(filterTableName, observationId, newObservationId))

    private def cloneGmosImagingMode(
      tableName: String,
      originalId: Observation.Id,
      newId: Observation.Id
    ): AppliedFragment =
      sql"""
        INSERT INTO #$tableName (
          c_observation_id,
          c_explicit_bin,
          c_explicit_amp_read_mode,
          c_explicit_amp_gain,
          c_explicit_roi
        )
        SELECT
          """.apply(Void) |+| sql"$observation_id".apply(newId) |+| sql""",
          c_explicit_bin,
          c_explicit_amp_read_mode,
          c_explicit_amp_gain,
          c_explicit_roi
        FROM #$tableName
        WHERE c_observation_id = """.apply(Void) |+| sql"$observation_id".apply(originalId)

    private def cloneGmosImagingFilters(
      tableName: String,
      originalId: Observation.Id,
      newId: Observation.Id
    ): AppliedFragment =
      sql"""
        INSERT INTO #$tableName (
          c_observation_id,
          c_filter
        )
        SELECT
          """.apply(Void) |+| sql"$observation_id".apply(newId) |+| sql""",
          c_filter
        FROM #$tableName
        WHERE c_observation_id = """.apply(Void) |+| sql"$observation_id".apply(originalId)


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

    def updateGmosImaging[F[_]: MonadCancelThrow, GF](
      modeTableName:         String,
      filterTableName:       String,
      commonUpdates:         GmosImagingInput.Edit.Common,
      filterUpdates:         Option[NonEmptyList[GF]],
      which:                 List[Observation.Id],
      insertFilterStatement: (Observation.Id, NonEmptyList[GF]) => AppliedFragment
    )(using Services[F]): F[Unit] =
      val modeUpdates = updateGmosImagingMode(modeTableName, commonUpdates, which).traverse(session.exec)
      val filterUpdatesEffect = filterUpdates.traverse: filters =>
          which.traverse_ { oid =>
            session.exec(deleteGmosImagingFilters(filterTableName, oid)) *>
              session.exec(insertFilterStatement(oid, filters)).whenA(filters.nonEmpty)
          }

      modeUpdates *> filterUpdatesEffect.void

    private def updateGmosImagingMode(
      tableName: String,
      input: GmosImagingInput.Edit.Common,
      which: List[Observation.Id]
    ): Option[AppliedFragment] =
      for {
        us <- NonEmptyList.fromList(commonUpdates(input))
        oids <- NonEmptyList.fromList(which)
      } yield
        sql"UPDATE #$tableName SET ".apply(Void) |+| us.intercalate(void", ") |+| void" WHERE " |+| observationIdIn(oids)

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
