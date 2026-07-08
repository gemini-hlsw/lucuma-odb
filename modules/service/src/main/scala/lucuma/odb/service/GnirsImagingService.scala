// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosInt
import grackle.Result
import grackle.ResultT
import grackle.syntax.*
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsReadMode
import lucuma.core.enums.GnirsWellDepth
import lucuma.core.enums.ImagingVariantType
import lucuma.core.enums.WavelengthOrder
import lucuma.core.math.Offset
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.odb.data.ExposureTimeModeId
import lucuma.odb.data.Nullable
import lucuma.odb.data.ObservingModeRowVersion
import lucuma.odb.data.TelescopeConfigGeneratorRole
import lucuma.odb.graphql.input.GnirsImagingInput
import lucuma.odb.graphql.input.ImagingVariantInput
import lucuma.odb.graphql.input.TelescopeConfigGeneratorInput
import lucuma.odb.sequence.data.TelescopeConfigGenerator
import lucuma.odb.sequence.gnirs.imaging.Config
import lucuma.odb.sequence.gnirs.imaging.Filter
import lucuma.odb.sequence.imaging.Variant
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GnirsCodecs.*
import monocle.Optional
import skunk.*
import skunk.codec.numeric.int4
import skunk.implicits.*

import Services.Syntax.*

sealed trait GnirsImagingService[F[_]]:

  def select(
    which: List[Observation.Id]
  ): F[Map[Observation.Id, Config]]

  def insert(
    input:  GnirsImagingInput.Create,
    reqEtm: Option[ExposureTimeMode],
    which:  List[Observation.Id]
  )(using Transaction[F]): F[Result[Unit]]

  def update(
    SET:   GnirsImagingInput.Edit,
    which: List[Observation.Id]
  )(using Transaction[F]): F[Result[Unit]]

  def delete(
    which: List[Observation.Id]
  )(using Transaction[F]): F[Unit]

  def clone(
    observationId:    Observation.Id,
    newObservationId: Observation.Id,
    etms:             List[(ExposureTimeModeId, ExposureTimeModeId)]
  )(using Transaction[F]): F[Unit]

object GnirsImagingService:

  private val ModeTableName   = "t_gnirs_imaging"
  private val FilterTableName = "t_gnirs_imaging_filter"

  def instantiate[F[_]: Concurrent](using Services[F]): GnirsImagingService[F] =
    new GnirsImagingService[F]:

      override def select(
        which: List[Observation.Id]
      ): F[Map[Observation.Id, Config]] =
        NonEmptyList
          .fromList(which)
          .fold(Map.empty[Observation.Id, Config].pure[F]): oids =>

            val precursorMap: F[Map[Observation.Id, (NonEmptyList[Filter], Variant.Fields, Statements.ModeFields)]] =
              val af = Statements.select(oids)
              session
                .prepareR(af.fragment.query(observation_id *: Statements.configFields))
                .use: pq =>
                  pq.stream(af.argument, chunkSize = 1024)
                    .compile
                    .toList
                    .map(_.map((oid, fs, vf, mf) => oid -> (fs, vf, mf)).toMap)

            for
              c <- precursorMap
              o <- services.telescopeConfigGeneratorService.select(oids, TelescopeConfigGeneratorRole.Object)
              s <- services.telescopeConfigGeneratorService.select(oids, TelescopeConfigGeneratorRole.Sky)
            yield c.view.map { case (oid, (fs, vf, mf)) =>
              val og = o.getOrElse(oid, TelescopeConfigGenerator.NoGenerator)
              val sg = s.getOrElse(oid, TelescopeConfigGenerator.NoGenerator)
              oid -> Config(
                vf.toVariant(og, sg),
                fs,
                mf.camera,
                mf.coadds,
                mf.explicitReadMode,
                mf.defaultWellDepth,
                mf.explicitWellDepth
              )
            }.toMap

      private def stripAcquisition[E](
        m: Map[Observation.Id, (E, NonEmptyList[(GnirsFilter, E)])]
      ): Map[Observation.Id, NonEmptyList[(GnirsFilter, E)]] =
        m.view.mapValues(_._2).toMap

      private def insertFilters(
        etms:    Map[Observation.Id, NonEmptyList[(GnirsFilter, ExposureTimeModeId)]],
        version: ObservingModeRowVersion
      ): F[Unit] =
        NonEmptyList
          .fromList:
            etms.toList.flatMap: (oid, fs) =>
              fs.toList.map: (filter, eid) =>
                (oid, filter, eid)
          .traverse_ : rs =>
            session.exec(ImagingStatements.insertFilters(GnirsImagingService.FilterTableName, gnirs_filter, rs, version))

      override def insert(
        input:  GnirsImagingInput.Create,
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
              r   <- ResultT(services.exposureTimeModeService.resolve("GNIRS Imaging", none, input.filters.map(f => (f.filter, f.exposureTimeMode)), reqEtm, which))

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

      override def update(
        SET:   GnirsImagingInput.Edit,
        which: List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        NonEmptyList.fromList(which).fold(().success.pure): oids =>

          val modeUpdates =
            NonEmptyList
              .fromList(
                Statements.commonUpdates(SET) ++
                SET.variant.toList.flatMap(ImagingStatements.variantUpdates)
              )
              .traverse_ : us =>
                session.exec:
                  sql"UPDATE #${GnirsImagingService.ModeTableName} SET "(Void) |+|
                    us.intercalate(void", ")                                   |+|
                    void" WHERE "                                              |+|
                    observationIdIn(oids)

          // Replace the current filters and their ETMs
          val filterUpdates =
            SET.filters.fold(ResultT.unit): fs =>
              for
                _   <- ResultT.liftF(session.exec(ImagingStatements.deleteCurrentFiltersAndEtms(GnirsImagingService.FilterTableName, oids)))
                // Insert the acquisition and science filters (current / mutable version)
                r   <- ResultT(services.exposureTimeModeService.resolve("GNIRS Imaging", none, fs.map(f => (f.filter, f.exposureTimeMode)), none, which))
                ids <- ResultT.liftF(services.exposureTimeModeService.insertResolvedAcquisitionAndScience(r))
                cur  = stripAcquisition(ids)
                _   <- ResultT.liftF(insertFilters(cur, ObservingModeRowVersion.Current))
              yield ()

          def updateOffsetForRole(
            input:   Nullable[TelescopeConfigGeneratorInput],
            variant: ImagingVariantType,
            role:    TelescopeConfigGeneratorRole
          ): F[Unit] =
            input.toOptionOption.fold(
              // the offset generator field was Absent, which means we should
              // default it to no generator when switching variants.
              services.telescopeConfigGeneratorService.resetWhenVariantNotMatching(
                oids,
                GnirsImagingService.ModeTableName,
                variant,
                role
              )
            ): in =>
              services.telescopeConfigGeneratorService.replace(oids, in, role)

          val offsetUpdates =
            SET.variant.fold(().pure[F]): v =>
              val (o, s) = v match
                case ImagingVariantInput.Grouped(_, offsets, _, skyOffsets, _)  => (offsets, skyOffsets)
                case ImagingVariantInput.Interleaved(offsets, _, skyOffsets) => (offsets, skyOffsets)
                case _                                                       => (Nullable.Null, Nullable.Null)
              updateOffsetForRole(o, v.variantType, TelescopeConfigGeneratorRole.Object) *>
              updateOffsetForRole(s, v.variantType, TelescopeConfigGeneratorRole.Sky)

          (for
            _ <- ResultT.liftF(offsetUpdates)
            _ <- ResultT.liftF(modeUpdates)
            _ <- filterUpdates
          yield ()).value

      override def clone(
        observationId:    Observation.Id,
        newObservationId: Observation.Id,
        etms:             List[(ExposureTimeModeId, ExposureTimeModeId)]
      )(using Transaction[F]): F[Unit] =
        session.exec(Statements.clone(observationId, newObservationId))                                                        *>
          session.exec(
            ImagingStatements.cloneFiltersAndEtms(GnirsImagingService.FilterTableName, observationId, newObservationId, etms)) *>
          services.telescopeConfigGeneratorService.clone(observationId, newObservationId)

  object Statements:

    // GNIRS imaging properties including overrides
    case class ModeFields(
      camera:            GnirsCamera,
      coadds:            PosInt,
      explicitReadMode:  Option[GnirsReadMode],
      defaultWellDepth:  GnirsWellDepth,
      explicitWellDepth: Option[GnirsWellDepth]
    )

    val modeFields: Decoder[ModeFields] =
      (gnirs_camera         *:
       int4_pos             *:
       gnirs_read_mode.opt  *:
       gnirs_well_depth     *:
       gnirs_well_depth.opt
      ).to[ModeFields]

    val configFields: Decoder[(NonEmptyList[Filter], Variant.Fields, ModeFields)] =
      (GmosImagingService.Statements.filter_list(gnirs_filter) *:
       GmosImagingService.Statements.variant_fields            *:
       modeFields
      ).map: (filters, variantFields, mode) =>
        (filters.map(f => Filter(f.filter, f.exposureTimeMode)), variantFields, mode)

    def select(
      oids: NonEmptyList[Observation.Id]
    ): AppliedFragment =
      sql"""
        WITH selected_observations AS (
          SELECT t.c_observation_id
          FROM #${GnirsImagingService.ModeTableName} t
          WHERE """(Void) |+| observationIdIn(oids) |+| sql"""
        ),
        aggregated_filters AS (
          SELECT
            f.c_observation_id,
            array_agg(
              ROW(
                f.c_filter,
                e.c_exposure_time_mode,
                e.c_signal_to_noise_at,
                e.c_signal_to_noise,
                e.c_exposure_time,
                e.c_exposure_count
              )::s_filter_exposure_time_mode
              ORDER BY f.c_filter
            ) AS c_filters
          FROM #${GnirsImagingService.FilterTableName} f
          JOIN t_exposure_time_mode e ON e.c_exposure_time_mode_id = f.c_exposure_time_mode_id
          JOIN selected_observations s ON s.c_observation_id = f.c_observation_id
          WHERE f.c_version = 'current'
          GROUP BY f.c_observation_id
        )
        SELECT
          v.c_observation_id,
          af.c_filters,
          #${ImagingStatements.variantColumns("v.")},
          v.c_camera,
          v.c_coadds,
          v.c_read_mode,
          v.c_well_depth_default,
          v.c_well_depth
        FROM v_gnirs_imaging v
        JOIN aggregated_filters af ON af.c_observation_id = v.c_observation_id
      """(Void)

    def insert(
      input: GnirsImagingInput.Create,
      which: NonEmptyList[Observation.Id]
    ): AppliedFragment =
      val modeEntries =
        which.map: oid =>
          sql"""(
            $observation_id,
            (SELECT c_program_id FROM t_observation WHERE c_observation_id = $observation_id),
            $gnirs_camera,
            $int4,
            ${gnirs_read_mode.opt},
            ${gnirs_well_depth.opt},
            $imaging_variant,
            $wavelength_order,
            $int4_nonneg,
            $int4_pos,
            $offset,
            $offset,
            $offset,
            $offset
          )"""(
            oid,
            oid,
            input.camera,
            input.coadds.value,
            input.explicitReadMode,
            input.explicitWellDepth,
            input.variant.variantType,
            ImagingVariantInput.order.getOption(input.variant).flatten.getOrElse(WavelengthOrder.Increasing),
            ImagingVariantInput.skyCount.getOption(input.variant).flatten.getOrElse(NonNegInt.MinValue),
            ImagingVariantInput.exposuresPerOffset.getOption(input.variant).flatten.getOrElse(PosInt.MinValue),
            ImagingVariantInput.preImaging.getOption(input.variant).flatMap(_.offset1).getOrElse(Offset.Zero),
            ImagingVariantInput.preImaging.getOption(input.variant).flatMap(_.offset2).getOrElse(Offset.Zero),
            ImagingVariantInput.preImaging.getOption(input.variant).flatMap(_.offset3).getOrElse(Offset.Zero),
            ImagingVariantInput.preImaging.getOption(input.variant).flatMap(_.offset4).getOrElse(Offset.Zero)
          )

      sql"""
        INSERT INTO #${GnirsImagingService.ModeTableName} (
          c_observation_id,
          c_program_id,
          c_camera,
          c_coadds,
          c_read_mode,
          c_well_depth,
          #${ImagingStatements.variantColumns()}
        ) VALUES
      """(Void) |+| modeEntries.intercalate(void", ")

    def delete(
      which: List[Observation.Id]
    ): AppliedFragment =
      sql"""
        DELETE FROM #${GnirsImagingService.ModeTableName}
        WHERE c_observation_id IN (
      """(Void) |+| which.map(sql"$observation_id").intercalate(void",") |+|
      void""")"""

    def commonUpdates(
      input: GnirsImagingInput.Edit
    ): List[AppliedFragment] =
      val upCamera    = sql"c_camera     = $gnirs_camera"
      val upCoadds    = sql"c_coadds     = $int4"
      val upReadMode  = sql"c_read_mode  = ${gnirs_read_mode.opt}"
      val upWellDepth = sql"c_well_depth = ${gnirs_well_depth.opt}"
      List(
        input.camera.map(upCamera),
        input.coadds.map(c => upCoadds(c.value)),
        input.explicitReadMode.toOptionOption.map(upReadMode),
        input.explicitWellDepth.toOptionOption.map(upWellDepth)
      ).flatten

    def clone(
      originalId: Observation.Id,
      newId:      Observation.Id
    ): AppliedFragment =
      sql"""
        INSERT INTO #${GnirsImagingService.ModeTableName} (
          c_observation_id,
          c_program_id,
          c_camera,
          c_coadds,
          c_read_mode,
          c_well_depth,
          #${ImagingStatements.variantColumns()}
        )
        SELECT
          $observation_id,
          (SELECT c_program_id FROM t_observation WHERE c_observation_id = $observation_id),
          c_camera,
          c_coadds,
          c_read_mode,
          c_well_depth,
          #${ImagingStatements.variantColumns()}
        FROM #${GnirsImagingService.ModeTableName}
        WHERE c_observation_id = $observation_id
      """.apply(newId, newId, originalId)
