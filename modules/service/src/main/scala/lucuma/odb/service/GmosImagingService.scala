// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Applicative
import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.all.*
import grackle.Result
import grackle.ResultT
import grackle.syntax.*
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.odb.data.ExposureTimeModeId
import lucuma.odb.data.ExposureTimeModeRole
import lucuma.odb.data.ObservingModeRowVersion
import lucuma.odb.data.OffsetGeneratorRole
import lucuma.odb.graphql.input.GmosImagingInput
import lucuma.odb.sequence.gmos.imaging.Config
import lucuma.odb.sequence.gmos.imaging.Variant
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GmosCodecs.*
import skunk.*
import skunk.codec.numeric.int8
import skunk.implicits.*

import Services.Syntax.*

sealed trait GmosImagingService[F[_]]:

  def selectNorth(
    which: List[Observation.Id]
  ): F[Map[Observation.Id, Config.GmosNorth]]

  def selectSouth(
    which: List[Observation.Id]
  ): F[Map[Observation.Id, Config.GmosSouth]]

  def selectNorthSeeds(
    oid: Observation.Id
  ): F[Map[GmosNorthFilter, Long]]

  def selectSouthSeeds(
    ooid: Observation.Id
  ): F[Map[GmosSouthFilter, Long]]

  def insertNorth(
    input:  GmosImagingInput.Create.North,
    reqEtm: Option[ExposureTimeMode],
    which:  List[Observation.Id]
  )(using Transaction[F]): F[Result[Unit]]

  def insertSouth(
    input:  GmosImagingInput.Create.South,
    reqEtm: Option[ExposureTimeMode],
    which:  List[Observation.Id]
  )(using Transaction[F]): F[Result[Unit]]

  def deleteNorth(
    which: List[Observation.Id]
  )(using Transaction[F]): F[Unit]

  def deleteSouth(
    which: List[Observation.Id]
  )(using Transaction[F]): F[Unit]

  def updateNorth(
    SET:   GmosImagingInput.Edit.North,
    which: List[Observation.Id]
  )(using Transaction[F]): F[Result[Unit]]

  def updateSouth(
    SET:   GmosImagingInput.Edit.South,
    which: List[Observation.Id]
  )(using Transaction[F]): F[Result[Unit]]

  def cloneNorth(
    observationId:    Observation.Id,
    newObservationId: Observation.Id,
    etms:             List[(ExposureTimeModeId, ExposureTimeModeId)]
  )(using Transaction[F]): F[Unit]

  def cloneSouth(
    observationId:    Observation.Id,
    newObservationId: Observation.Id,
    etms:             List[(ExposureTimeModeId, ExposureTimeModeId)]
  )(using Transaction[F]): F[Unit]

object GmosImagingService:

  def instantiate[F[_]: Concurrent](using Services[F]): GmosImagingService[F] =
    new GmosImagingService[F]:

      override def selectNorth(
        which: List[Observation.Id]
      ): F[Map[Observation.Id, Config.GmosNorth]] =
        select("v_gmos_north_imaging", which, Statements.north)

      override def selectSouth(
        which: List[Observation.Id]
      ): F[Map[Observation.Id, Config.GmosSouth]] =
        select("v_gmos_south_imaging", which, Statements.south)

      private def select[A](
        view:    String,
        which:   List[Observation.Id],
        decoder: Decoder[A]
      ): F[Map[Observation.Id, A]] =
        NonEmptyList
          .fromList(which)
          .fold(Applicative[F].pure(List.empty)): oids =>
            val af = Statements.select(view, oids)
            session.prepareR(af.fragment.query(observation_id *: decoder)).use: pq =>
              pq.stream(af.argument, chunkSize = 1024).compile.toList
          .map(_.toMap)

      override def selectNorthSeeds(
        oid: Observation.Id
      ): F[Map[GmosNorthFilter, Long]] =
        selectSeeds[GmosNorthFilter]("t_gmos_north_imaging_filter", oid, gmos_north_filter)

      override def selectSouthSeeds(
        oid: Observation.Id
      ): F[Map[GmosSouthFilter, Long]] =
        selectSeeds[GmosSouthFilter]("t_gmos_south_imaging_filter", oid, gmos_south_filter)

      private def selectSeeds[A](
        table:   String,
        oid:     Observation.Id,
        decoder: Decoder[A]
      ): F[Map[A, Long]] =
        val af = Statements.selectSeeds(table, oid)
        session.prepareR(af.fragment.query(decoder *: int8)).use: pq =>
          pq.stream(af.argument, chunkSize=64)
            .compile
            .toList
            .map(_.toMap)


      override def insertNorth(
        input:  GmosImagingInput.Create.North,
        reqEtm: Option[ExposureTimeMode],
        which:  List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        insert(
          "GMOS North Imaging",
          "t_gmos_north_imaging",
          "t_gmos_north_imaging_filter",
          gmos_north_filter,
          input,
          reqEtm,
          which
        )

      override def insertSouth(
        input:  GmosImagingInput.Create.South,
        reqEtm: Option[ExposureTimeMode],
        which:  List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        insert(
          "GMOS South Imaging",
          "t_gmos_south_imaging",
          "t_gmos_south_imaging_filter",
          gmos_south_filter,
          input,
          reqEtm,
          which
        )

      private def insert[L](
        modeName:    String,
        modeTable:   String,
        filterTable: String,
        filterCodec: Codec[L],
        input:       GmosImagingInput.Create[L],
        reqEtm:      Option[ExposureTimeMode],
        which:       List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        NonEmptyList
          .fromList(which)
          .fold(ResultT.unit[F]): oids =>
            for
              _   <- ResultT.liftF(session.exec(Statements.insert(modeTable, input.common, oids)))

              // Resolve the exposure time modes for acquisition and science
              r   <- ResultT(services.exposureTimeModeService.resolve(modeName, none, input.filters.map(f => (f.filter, f.exposureTimeMode)), reqEtm, which))

              // Insert the acquisition and science filters (initial / immutable version)
              ids <- ResultT.liftF(services.exposureTimeModeService.insertResolvedAcquisitionAndScience(r))
              ini  = stripAcquisition(ids)
              _   <- ResultT.liftF(insertFilters(ini, filterTable, filterCodec, ObservingModeRowVersion.Initial))

              // Insert the science filters (current / mutable version)
              cur <- ResultT.liftF(services.exposureTimeModeService.insertResolvedScienceOnly(stripAcquisition(r)))
              _   <- ResultT.liftF(insertFilters(cur, filterTable, filterCodec, ObservingModeRowVersion.Current))

              // Insert the offset generators
              _  <- ResultT.liftF:
                      Variant.offsets.getOption(input.common.variant).traverse_ : og =>
                        services.offsetGeneratorService.insert(oids, og, OffsetGeneratorRole.Object)

              _  <- ResultT.liftF:
                      Variant.skyOffsets.getOption(input.common.variant).traverse_ : og =>
                        services.offsetGeneratorService.insert(oids, og, OffsetGeneratorRole.Sky)

            yield ()
          .value

      private def stripAcquisition[L, E](
        m: Map[Observation.Id, (E, NonEmptyList[(L, E)])]
      ): Map[Observation.Id, NonEmptyList[(L, E)]] =
        m.view.mapValues(_._2).toMap

      private def insertFilters[L](
        etms:        Map[Observation.Id, NonEmptyList[(L, ExposureTimeModeId)]],
        filterTable: String,
        filterCodec: Codec[L],
        version:     ObservingModeRowVersion
      ): F[Unit] =
        NonEmptyList
          .fromList:
            etms.toList.flatMap: (oid, fs) =>
              fs.toList.map: (filter, eid) =>
                (oid, filter, eid)
          .traverse_ : rs =>
            session.exec(Statements.insertFilters(filterTable, filterCodec, rs, version))

      override def deleteNorth(
        which: List[Observation.Id]
      )(using Transaction[F]): F[Unit] =
        delete("t_gmos_north_imaging", which)

      override def deleteSouth(
        which: List[Observation.Id]
      )(using Transaction[F]): F[Unit] =
        delete("t_gmos_south_imaging", which)

      private def delete(
        modeTable: String,
        which:     List[Observation.Id]
      )(using Transaction[F]): F[Unit] =
        services
          .exposureTimeModeService
          .deleteMany(which, ExposureTimeModeRole.Acquisition, ExposureTimeModeRole.Science)
          .productR:
            session.exec(Statements.delete(modeTable, which))

      override def updateNorth(
        SET: GmosImagingInput.Edit.North,
        which: List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        update(
          "GMOS North Imaging",
          "t_gmos_north_imaging",
          "t_gmos_north_imaging_filter",
          gmos_north_filter,
          SET,
          none,
          which
        )

      override def updateSouth(
        SET: GmosImagingInput.Edit.South,
        which: List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        update(
          "GMOS South Imaging",
          "t_gmos_south_imaging",
          "t_gmos_south_imaging_filter",
          gmos_south_filter,
          SET,
          none,
          which
        )

      private def update[L](
        modeName:       String,
        modeTable:      String,
        filterTable:    String,
        filterCodec:    Codec[L],
        edit:           GmosImagingInput.Edit[L],
        newRequirement: Option[ExposureTimeMode],
        which:          List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =

        NonEmptyList.fromList(which).fold(().success.pure): oids =>
          val modeUpdates =
            NonEmptyList
              .fromList(Statements.commonUpdates(edit.common))
              .traverse_ : us =>
                session.exec:
                  sql"UPDATE #$modeTable SET "(Void) |+|
                    us.intercalate(void", ")         |+|
                    void" WHERE "                    |+|
                    observationIdIn(oids)

          val filterUpdates =
            edit.filters.fold(ResultT.unit): fs =>
              for
                _ <- ResultT.liftF(session.exec(Statements.deleteCurrentFiltersAndEtms(filterTable, oids)))

                // Resolve the exposure time modes for acquisition and science
                r   <- ResultT(services.exposureTimeModeService.resolve(modeName, none, fs.map(f => (f.filter, f.exposureTimeMode)), newRequirement, which))

                // Insert the acquisition and science filters (current / mutable version)
                ids <- ResultT.liftF(services.exposureTimeModeService.insertResolvedAcquisitionAndScience(r))
                cur  = stripAcquisition(ids)
                _   <- ResultT.liftF(insertFilters(cur, filterTable, filterCodec, ObservingModeRowVersion.Current))
              yield ()

//          def updateOffsetForRole(
//            input: Nullable[OffsetGeneratorInput],
//            role:  OffsetGeneratorRole
//          ): F[Unit] =
//            input.toOptionOption.fold(Concurrent[F].unit): in =>
//              services.offsetGeneratorService.replace(oids, in, role)
//
//          val offsetUpdates =
//            updateOffsetForRole(edit.common.objectOffsetGenerator, OffsetGeneratorRole.Object) *>
//            updateOffsetForRole(edit.common.skyOffsetGenerator, OffsetGeneratorRole.Sky)

          (for
            _ <- ResultT.liftF(modeUpdates)
            _ <- filterUpdates
//            _ <- ResultT.liftF(offsetUpdates)
          yield ()).value

      override def cloneNorth(
        observationId:    Observation.Id,
        newObservationId: Observation.Id,
        etms:             List[(ExposureTimeModeId, ExposureTimeModeId)]
      )(using Transaction[F]): F[Unit] =
        clone("t_gmos_north_imaging", "t_gmos_north_imaging_filter", observationId, newObservationId, etms)

      override def cloneSouth(
        observationId:    Observation.Id,
        newObservationId: Observation.Id,
        etms:             List[(ExposureTimeModeId, ExposureTimeModeId)]
      )(using Transaction[F]): F[Unit] =
        clone("t_gmos_south_imaging", "t_gmos_south_imaging_filter", observationId, newObservationId, etms)

      private def clone(
        modeTableName:    String,
        filterTableName:  String,
        observationId:    Observation.Id,
        newObservationId: Observation.Id,
        etms:             List[(ExposureTimeModeId, ExposureTimeModeId)]
      )(using Services[F]): F[Unit] =
        session.exec(Statements.clone(modeTableName, observationId, newObservationId))                       *>
        session.exec(Statements.cloneFiltersAndEtms(filterTableName, observationId, newObservationId, etms)) *>
        services.offsetGeneratorService.clone(observationId, newObservationId)

  object Statements:

    val common: Decoder[Config.Common] =
      (
        gmos_binning              *:
        gmos_binning.opt          *:
        gmos_amp_read_mode.opt    *:
        gmos_amp_gain.opt         *:
        gmos_roi.opt
      ).map: (defaultBin, explicitBin, arm, ag, roi) =>
        Config.Common(defaultBin, explicitBin, arm, ag, roi)

    val north: Decoder[Config.GmosNorth] =
      (_gmos_north_filter *:
       common
      ).emap { case (f, c) =>
        NonEmptyList.fromList(f.toList).fold(
          "Filters list cannot be empty".asLeft
        )(Config.GmosNorth(_, c).asRight)
      }

    val south: Decoder[Config.GmosSouth] =
      (_gmos_south_filter *:
       common
      ).emap { case (f, c) =>
        NonEmptyList.fromList(f.toList).fold(
          "Filters list cannot be empty".asLeft
        )(Config.GmosSouth(_, c).asRight)
      }

    def select(
      viewName: String,
      oids:     NonEmptyList[Observation.Id]
    ): AppliedFragment =
      sql"""
        SELECT
          c_observation_id,
          c_filters,
          c_bin_default,
          c_bin,
          c_amp_read_mode,
          c_amp_gain,
          c_roi
        FROM #$viewName
        WHERE
      """(Void) |+| observationIdIn(oids)

    def selectSeeds(
      tableName: String,
      oid:       Observation.Id
    ): AppliedFragment =
      sql"""
        SELECT
          c_filter,
          c_seed
        FROM #$tableName
        WHERE c_observation_id = $observation_id
      """.apply(oid)

    def insert(
      modeTable: String,
      common:    GmosImagingInput.Create.Common,
      which:     NonEmptyList[Observation.Id]
    ): AppliedFragment =
      val modeEntries =
        which.map: oid =>
          sql"""(
            $observation_id,
            ${gmos_binning.opt},
            ${gmos_amp_read_mode.opt},
            ${gmos_amp_gain.opt},
            ${gmos_roi.opt},
            $gmos_imaging_type,
            $wavelength_order,
            $int4_nonneg,
            $offset,
            $offset,
            $offset,
            $offset
          )"""(
            oid,
            common.explicitBin,
            common.explicitAmpReadMode,
            common.explicitAmpGain,
            common.explicitRoi,
            common.variant.variantType,
            common.variant.toGrouped.order,
            common.variant.toGrouped.skyCount,
            common.variant.toPreImaging.offset1,
            common.variant.toPreImaging.offset2,
            common.variant.toPreImaging.offset3,
            common.variant.toPreImaging.offset4,
          )

      sql"""
        INSERT INTO #$modeTable (
          c_observation_id,
          c_bin,
          c_amp_read_mode,
          c_amp_gain,
          c_roi,
          c_imaging_type,
          c_wavelength_order,
          c_sky_count,
          c_pre_imaging_off1_p,
          c_pre_imaging_off1_q,
          c_pre_imaging_off2_p,
          c_pre_imaging_off2_q,
          c_pre_imaging_off3_p,
          c_pre_imaging_off3_q,
          c_pre_imaging_off4_p,
          c_pre_imaging_off4_q
        ) VALUES
      """(Void) |+| modeEntries.intercalate(void", ")

    def delete(
      tableName: String,
      which:     List[Observation.Id]
    ): AppliedFragment =
      sql"""
        DELETE FROM #$tableName
        WHERE c_observation_id IN (
      """(Void) |+| which.map(sql"$observation_id").intercalate(void",") |+|
      void""")"""

    def clone(
      tableName: String,
      originalId: Observation.Id,
      newId: Observation.Id
    ): AppliedFragment =
      sql"""
        INSERT INTO #$tableName (
          c_observation_id,
          c_bin,
          c_amp_read_mode,
          c_amp_gain,
          c_roi
        )
        SELECT
          """.apply(Void) |+| sql"$observation_id".apply(newId) |+| sql""",
          c_bin,
          c_amp_read_mode,
          c_amp_gain,
          c_roi
        FROM #$tableName
        WHERE c_observation_id = """.apply(Void) |+| sql"$observation_id".apply(originalId)

    def cloneFiltersAndEtms(
      tableName:  String,
      originalId: Observation.Id,
      newId:      Observation.Id,
      etms:       List[(ExposureTimeModeId, ExposureTimeModeId)]
    ): AppliedFragment =
      sql"""
        WITH etm_map AS (
          SELECT
            old_exposure_time_mode_id,
            new_exposure_time_mode_id
          FROM
            unnest(
              ARRAY[${exposure_time_mode_id.list(etms.length)}],
              ARRAY[${exposure_time_mode_id.list(etms.length)}]
            ) AS map(old_exposure_time_mode_id, new_exposure_time_mode_id)
        )
        INSERT INTO #$tableName (
          c_observation_id,
          c_exposure_time_mode_id,
          c_filter,
          c_version,
          c_role
        )
        SELECT
          $observation_id,
          e.new_exposure_time_mode_id,
          f.c_filter,
          f.c_version,
          f.c_role
        FROM #$tableName f
        JOIN etm_map e ON f.c_exposure_time_mode_id = e.old_exposure_time_mode_id
        WHERE f.c_observation_id = $observation_id
      """.apply(etms.map(_._1), etms.map(_._2), newId, originalId)

    // Update statements following the GmosLongSlitService pattern
    def commonUpdates(
      input: GmosImagingInput.Edit.Common
    ): List[AppliedFragment] =
      val upBin = sql"c_bin = ${gmos_binning.opt}"
      val upAmpReadMode = sql"c_amp_read_mode = ${gmos_amp_read_mode.opt}"
      val upAmpGain = sql"c_amp_gain = ${gmos_amp_gain.opt}"
      val upRoi = sql"c_roi = ${gmos_roi.opt}"
      List(
        input.explicitBin.toOptionOption.map(upBin),
        input.explicitAmpReadMode.toOptionOption.map(upAmpReadMode),
        input.explicitAmpGain.toOptionOption.map(upAmpGain),
        input.explicitRoi.toOptionOption.map(upRoi)
      ).flatten

    def deleteCurrentFiltersAndEtms(
      tableName: String,
      which:     NonEmptyList[Observation.Id]
    ): AppliedFragment =
      // deletion cascades to the filters
      sql"""
        DELETE FROM t_exposure_time_mode m
          WHERE m.c_observation_id IN ${observation_id.list(which.length).values}
          AND (
            m.c_role = $exposure_time_mode_role OR
            (
              m.c_role = $exposure_time_mode_role AND
              EXISTS (
                SELECT 1
                FROM #$tableName f
                WHERE f.c_exposure_time_mode_id = m.c_exposure_time_mode_id
                  AND f.c_version = ${observing_mode_row_version}
              )
            )
          )
      """.apply(
        which.toList,
        ExposureTimeModeRole.Acquisition,
        ExposureTimeModeRole.Science,
        ObservingModeRowVersion.Current
      )

    def insertFilters[L](
      tableName:   String,
      filterCodec: Codec[L],
      rows:        NonEmptyList[(Observation.Id, L, ExposureTimeModeId)],
      version:     ObservingModeRowVersion
    ): AppliedFragment =
      def insertFilters: AppliedFragment =
        sql"""
          INSERT INTO #$tableName (
            c_observation_id,
            c_filter,
            c_version,
            c_exposure_time_mode_id
          ) VALUES
        """(Void)

      def filterEntries =
        rows.map: (oid, filter, eid) =>
          sql"""($observation_id, $filterCodec, $observing_mode_row_version, $exposure_time_mode_id)"""(oid, filter, version, eid)

      val filterValues: AppliedFragment = filterEntries.intercalate(void", ")

      insertFilters |+| filterValues