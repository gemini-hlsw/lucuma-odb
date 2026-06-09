// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import grackle.Result
import grackle.ResultT
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.WavelengthOrder
import lucuma.core.math.Offset
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.odb.data.ExposureTimeModeId
import lucuma.odb.data.Nullable
import lucuma.odb.data.ObservingModeRowVersion
import lucuma.odb.data.TelescopeConfigGeneratorRole
import lucuma.odb.graphql.input.Flamingos2ImagingInput
import lucuma.odb.graphql.input.ImagingVariantInput
import lucuma.odb.graphql.input.TelescopeConfigGeneratorInput
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.Flamingos2Codecs.*
import monocle.Optional
import skunk.*
import skunk.implicits.*

import Services.Syntax.*

sealed trait Flamingos2ImagingService[F[_]]:

  def insert(
    input:  Flamingos2ImagingInput.Create,
    reqEtm: Option[ExposureTimeMode],
    which:  List[Observation.Id]
  )(using Transaction[F]): F[Result[Unit]]

  def delete(
    which: List[Observation.Id]
  )(using Transaction[F]): F[Unit]

  def clone(
    observationId:    Observation.Id,
    newObservationId: Observation.Id,
    etms:             List[(ExposureTimeModeId, ExposureTimeModeId)]
  )(using Transaction[F]): F[Unit]

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

        def offsetInput(
          in: Optional[ImagingVariantInput, Nullable[TelescopeConfigGeneratorInput]]
        ): TelescopeConfigGeneratorInput =
          in.getOption(input.variant).flatMap(_.toOption).getOrElse(TelescopeConfigGeneratorInput.NoGeneratorInput)

        val offsets    = offsetInput(ImagingVariantInput.offsets)
        val skyOffsets = offsetInput(ImagingVariantInput.skyOffsets)

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

              // Insert the offset generators
              _   <- ResultT.liftF(services.telescopeConfigGeneratorService.insert(oids, offsets, TelescopeConfigGeneratorRole.Object))
              _   <- ResultT.liftF(services.telescopeConfigGeneratorService.insert(oids, skyOffsets, TelescopeConfigGeneratorRole.Sky))
            yield ()
          .value

      override def delete(
        which: List[Observation.Id]
      )(using Transaction[F]): F[Unit] =
        session.exec(Statements.delete(which))

      override def clone(
        observationId:    Observation.Id,
        newObservationId: Observation.Id,
        etms:             List[(ExposureTimeModeId, ExposureTimeModeId)]
      )(using Transaction[F]): F[Unit] =
        session.exec(Statements.clone(observationId, newObservationId))                       *>
          session.exec(Statements.cloneFiltersAndEtms(observationId, newObservationId, etms)) *>
          services.telescopeConfigGeneratorService.clone(observationId, newObservationId)

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
            $imaging_variant,
            $wavelength_order,
            $int4_nonneg,
            $offset,
            $offset,
            $offset,
            $offset
          )"""(
            oid,
            oid,
            input.explicitReadMode,
            input.explicitReads,
            input.explicitDecker,
            input.explicitReadoutMode,
            input.variant.variantType,
            ImagingVariantInput.order.getOption(input.variant).flatten.getOrElse(WavelengthOrder.Increasing),
            ImagingVariantInput.skyCount.getOption(input.variant).flatten.getOrElse(NonNegInt.MinValue),
            ImagingVariantInput.preImaging.getOption(input.variant).flatMap(_.offset1).getOrElse(Offset.Zero),
            ImagingVariantInput.preImaging.getOption(input.variant).flatMap(_.offset2).getOrElse(Offset.Zero),
            ImagingVariantInput.preImaging.getOption(input.variant).flatMap(_.offset3).getOrElse(Offset.Zero),
            ImagingVariantInput.preImaging.getOption(input.variant).flatMap(_.offset4).getOrElse(Offset.Zero)
          )

      sql"""
        INSERT INTO #${Flamingos2ImagingService.ModeTableName} (
          c_observation_id,
          c_program_id,
          c_read_mode,
          c_reads,
          c_decker,
          c_readout_mode,
          c_variant,
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
      which: List[Observation.Id]
    ): AppliedFragment =
      sql"""
        DELETE FROM #${Flamingos2ImagingService.ModeTableName}
        WHERE c_observation_id IN (
      """(Void) |+| which.map(sql"$observation_id").intercalate(void",") |+|
      void""")"""

    def clone(
      originalId: Observation.Id,
      newId:      Observation.Id
    ): AppliedFragment =
      sql"""
        INSERT INTO #${Flamingos2ImagingService.ModeTableName} (
          c_observation_id,
          c_program_id,
          c_read_mode,
          c_reads,
          c_decker,
          c_readout_mode,
          c_variant,
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
        )
        SELECT
          $observation_id,
          (SELECT c_program_id FROM t_observation WHERE c_observation_id = $observation_id),
          c_read_mode,
          c_reads,
          c_decker,
          c_readout_mode,
          c_variant,
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
        FROM #${Flamingos2ImagingService.ModeTableName}
        WHERE c_observation_id = $observation_id
      """.apply(newId, newId, originalId)

    def cloneFiltersAndEtms(
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
        INSERT INTO #${Flamingos2ImagingService.FilterTableName} (
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
        FROM #${Flamingos2ImagingService.FilterTableName} f
        JOIN etm_map e ON f.c_exposure_time_mode_id = e.old_exposure_time_mode_id
        WHERE f.c_observation_id = $observation_id
      """.apply(etms.map(_._1), etms.map(_._2), newId, originalId)

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
