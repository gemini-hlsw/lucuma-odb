// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.all.*
import grackle.Result
import grackle.ResultT
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.odb.data.ExposureTimeModeId
import lucuma.odb.data.ObservingModeRowVersion
import lucuma.odb.graphql.input.Flamingos2ImagingInput
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.Flamingos2Codecs.*
import skunk.*
import skunk.codec.text.text
import skunk.implicits.*

import Services.Syntax.*

sealed trait Flamingos2ImagingService[F[_]]:

  def insert(
    input:  Flamingos2ImagingInput.Create,
    reqEtm: Option[ExposureTimeMode],
    which:  List[Observation.Id]
  )(using Transaction[F]): F[Result[Unit]]

object Flamingos2ImagingService:

  private val ModeTableName   = "t_flamingos_2_imaging"
  private val FilterTableName = "t_flamingos_2_imaging_filter"

  def instantiate[F[_]: Concurrent](using Services[F]): Flamingos2ImagingService[F] =
    new Flamingos2ImagingService[F]:

      private def stripAcquisition[E](
        m: Map[Observation.Id, (E, NonEmptyList[(Flamingos2Filter, E)])]
      ): Map[Observation.Id, NonEmptyList[(Flamingos2Filter, E)]] =
        m.view.mapValues(_._2).toMap

      private def insertFilters(
        etms:    Map[Observation.Id, NonEmptyList[(Flamingos2Filter, ExposureTimeModeId)]],
        version: ObservingModeRowVersion
      ): F[Unit] =
        NonEmptyList
          .fromList:
            etms.toList.flatMap: (oid, fs) =>
              fs.toList.map: (filter, eid) =>
                (oid, filter, eid)
          .traverse_ : rs =>
            session.exec(Statements.insertFilters(rs, version))

      override def insert(
        input:  Flamingos2ImagingInput.Create,
        reqEtm: Option[ExposureTimeMode],
        which:  List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        NonEmptyList
          .fromList(which)
          .fold(ResultT.unit[F]): oids =>
            for
              _   <- ResultT.liftF(session.exec(Statements.insert(input, oids)))

              // Resolve the etms for acquisition and science
              r   <- ResultT(services.exposureTimeModeService.resolve("Flamingos2 Imaging", none, input.filters.map(f => (f.filter, f.exposureTimeMode)), reqEtm, which))

              ids <- ResultT.liftF(services.exposureTimeModeService.insertResolvedAcquisitionAndScience(r))
              ini  = stripAcquisition(ids)
              _   <- ResultT.liftF(insertFilters(ini, ObservingModeRowVersion.Initial))

              // Insert the science filters
              cur <- ResultT.liftF(services.exposureTimeModeService.insertResolvedScienceOnly(stripAcquisition(r)))
              _   <- ResultT.liftF(insertFilters(cur, ObservingModeRowVersion.Current))
            yield ()
          .value

  object Statements:

    def insert(
      input: Flamingos2ImagingInput.Create,
      which: NonEmptyList[Observation.Id]
    ): AppliedFragment =
      val modeEntries =
        which.map: oid =>
          sql"""(
            $observation_id,
            (SELECT c_program_id FROM t_observation WHERE c_observation_id = $observation_id),
            ${flamingos_2_read_mode.opt},
            ${flamingos_2_reads.opt},
            ${flamingos_2_decker.opt},
            ${flamingos_2_readout_mode.opt},
            ${text.opt}
          )"""(
            oid,
            oid,
            input.explicitReadMode,
            input.explicitReads,
            input.explicitDecker,
            input.explicitReadoutMode,
            input.formattedOffsets
          )

      sql"""
        INSERT INTO #${Flamingos2ImagingService.ModeTableName} (
          c_observation_id,
          c_program_id,
          c_read_mode,
          c_reads,
          c_decker,
          c_readout_mode,
          c_offsets
        ) VALUES
      """(Void) |+| modeEntries.intercalate(void", ")

    def insertFilters(
      rows:    NonEmptyList[(Observation.Id, Flamingos2Filter, ExposureTimeModeId)],
      version: ObservingModeRowVersion
    ): AppliedFragment =
      val insertInto: AppliedFragment =
        sql"""
          INSERT INTO #${Flamingos2ImagingService.FilterTableName} (
            c_observation_id,
            c_filter,
            c_version,
            c_exposure_time_mode_id
          ) VALUES
        """(Void)

      val filterEntries =
        rows.map: (oid, filter, eid) =>
          sql"""($observation_id, $flamingos_2_filter, $observing_mode_row_version, $exposure_time_mode_id)"""(oid, filter, version, eid)

      insertInto |+| filterEntries.intercalate(void", ")
