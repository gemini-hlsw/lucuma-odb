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
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.core.model.sequence.gnirs.GnirsAcquisitionMode
import lucuma.odb.data.ExposureTimeModeId
import lucuma.odb.data.ExposureTimeModeRole
import lucuma.odb.data.Nullable
import lucuma.odb.data.ObservingModeRowVersion
import lucuma.odb.data.TelescopeConfigGeneratorRole
import lucuma.odb.graphql.input.GnirsImagingFilterInput
import lucuma.odb.graphql.input.GnirsImagingInput
import lucuma.odb.graphql.input.ImagingVariantInput
import lucuma.odb.graphql.input.TelescopeConfigGeneratorInput
import lucuma.odb.sequence.data.TelescopeConfigGenerator
import lucuma.odb.sequence.gnirs.AcquisitionConfig
import lucuma.odb.sequence.gnirs.imaging.Config
import lucuma.odb.sequence.gnirs.imaging.Filter
import lucuma.odb.sequence.imaging.Variant
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GnirsCodecs.*
import monocle.Optional
import skunk.*
import skunk.codec.numeric.int4
import skunk.codec.numeric.int8
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
                    .map: rows =>
                      // One row per filter, ordered by filter.  `groupBy` preserves
                      // that order within each group.
                      rows
                        .groupBy(_._1)
                        .flatMap: (oid, group) =>
                          NonEmptyList.fromList(group.map(_._2)).map: fs =>
                            oid -> (fs, group.head._3, group.head._4)
                        .toMap

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
                mf.explicitReadMode,
                mf.defaultWellDepth,
                mf.explicitWellDepth,
                mf.acquisition
              )
            }.toMap

      private def stripAcquisition[E](
        m: Map[Observation.Id, (E, NonEmptyList[(GnirsFilter, E)])]
      ): Map[Observation.Id, NonEmptyList[(GnirsFilter, E)]] =
        m.view.mapValues(_._2).toMap

      /**
       * Writes the filter rows for one row version.  The ETM resolution carries
       * only the filter, so the coadds are re-attached here from the input.
       */
      private def insertFilters(
        input:   NonEmptyList[GnirsImagingFilterInput],
        etms:    Map[Observation.Id, NonEmptyList[(GnirsFilter, ExposureTimeModeId)]],
        version: ObservingModeRowVersion
      ): F[Unit] =
        val coaddsFor: Map[GnirsFilter, PosInt] =
          input.toList.map(f => f.filter -> f.coadds.getOrElse(GnirsImagingInput.DefaultCoadds)).toMap
        NonEmptyList
          .fromList:
            etms.toList.flatMap: (oid, fs) =>
              fs.toList.map: (filter, eid) =>
                (oid, filter, coaddsFor.getOrElse(filter, GnirsImagingInput.DefaultCoadds), eid)
          .traverse_ : rs =>
            session.exec(Statements.insertFilters(rs, version))

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

              // Resolve the etms for acquisition and science. An explicit acquisition ETM
              // wins; otherwise it is derived from the first science ETM.
              r   <- ResultT(services.exposureTimeModeService.resolve("GNIRS Imaging", input.acquisition.flatMap(_.exposureTimeMode), input.filters.map(f => (f.filter, f.exposureTimeMode)), reqEtm, which))

              ids <- ResultT.liftF(services.exposureTimeModeService.insertResolvedAcquisitionAndScience(r))
              ini  = stripAcquisition(ids)
              _   <- ResultT.liftF(insertFilters(input.filters, ini, ObservingModeRowVersion.Initial))

              // Insert the science filters
              cur <- ResultT.liftF(services.exposureTimeModeService.insertResolvedScienceOnly(stripAcquisition(r)))
              _   <- ResultT.liftF(insertFilters(input.filters, cur, ObservingModeRowVersion.Current))

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

          // Replace the current filters and their science ETMs. The acquisition ETM is
          // user-editable, so unlike the other imaging modes it is left in place here and
          // only changed when the input asks for it.
          val filterUpdates =
            SET.filters.fold(ResultT.unit): fs =>
              for
                _   <- ResultT.liftF(session.exec(ImagingStatements.deleteCurrentScienceFiltersAndEtms(GnirsImagingService.FilterTableName, oids)))
                // Insert the science filters (current / mutable version)
                r   <- ResultT(services.exposureTimeModeService.resolve("GNIRS Imaging", none, fs.map(f => (f.filter, f.exposureTimeMode)), none, which))
                cur <- ResultT.liftF(services.exposureTimeModeService.insertResolvedScienceOnly(stripAcquisition(r)))
                _   <- ResultT.liftF(insertFilters(fs, cur, ObservingModeRowVersion.Current))
              yield ()

          val acqEtmUpdate =
            SET.acquisition.flatMap(_.exposureTimeMode).fold(().pure[F]): e =>
              services.exposureTimeModeService.updateMany(which, ExposureTimeModeRole.Acquisition, e)

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
                case ImagingVariantInput.Grouped(_, offsets, _, skyOffsets)  => (offsets, skyOffsets)
                case ImagingVariantInput.Interleaved(offsets, _, skyOffsets) => (offsets, skyOffsets)
                case _                                                       => (Nullable.Null, Nullable.Null)
              updateOffsetForRole(o, v.variantType, TelescopeConfigGeneratorRole.Object) *>
              updateOffsetForRole(s, v.variantType, TelescopeConfigGeneratorRole.Sky)

          (for
            _ <- ResultT.liftF(offsetUpdates)
            _ <- ResultT.liftF(modeUpdates)
            _ <- filterUpdates
            _ <- ResultT.liftF(acqEtmUpdate)
          yield ()).value

      override def clone(
        observationId:    Observation.Id,
        newObservationId: Observation.Id,
        etms:             List[(ExposureTimeModeId, ExposureTimeModeId)]
      )(using Transaction[F]): F[Unit] =
        session.exec(Statements.clone(observationId, newObservationId))                                *>
          session.exec(Statements.cloneFiltersAndEtms(observationId, newObservationId, etms))          *>
          services.telescopeConfigGeneratorService.clone(observationId, newObservationId)

  object Statements:

    // GNIRS imaging properties including overrides
    case class ModeFields(
      camera:            GnirsCamera,
      explicitReadMode:  Option[GnirsReadMode],
      defaultWellDepth:  GnirsWellDepth,
      explicitWellDepth: Option[GnirsWellDepth],
      acquisition:       AcquisitionConfig
    )

    // Inline acquisition columns plus the acquisition ETM joined from
    // t_exposure_time_mode. The sky offset is present only for an explicit FAINT type
    // (DB CHECK enforced) and is carried inside the Faint mode; the filter override is
    // separate.
    val acquisition: Decoder[AcquisitionConfig] =
      (gnirs_acquisition_type.opt *: // c_acq_type (None => AUTO mode)
       int4_pos                   *: // c_acq_coadds
       gnirs_filter.opt           *: // c_acq_filter (explicit override; None => first science filter)
       angle_µas.opt              *: // c_acq_sky_offset_p
       angle_µas.opt              *: // c_acq_sky_offset_q
       exposure_time_mode            // acquisition ETM
      ).map: (acqType, acqCoadds, acqFilter, acqSkyOffP, acqSkyOffQ, acqEtm) =>
        val acqSkyOffset: Offset =
          (acqSkyOffP, acqSkyOffQ)
            .mapN((p, q) => Offset(Offset.P(p), Offset.Q(q)))
            .getOrElse(GnirsAcquisitionMode.Faint.DefaultImagingSkyOffset)
        val explicitAcqMode: Option[GnirsAcquisitionMode] =
          acqType.map(GnirsAcquisitionMode.forTypeAndOffset(_, acqSkyOffset))
        AcquisitionConfig(explicitAcqMode, acqFilter, acqEtm, acqCoadds)

    val modeFields: Decoder[ModeFields] =
      (gnirs_camera         *:
       gnirs_read_mode.opt  *:
       gnirs_well_depth     *:
       gnirs_well_depth.opt *:
       acquisition
      ).to[ModeFields]

    /**
     * Decodes one (observation, filter) row.  `select` groups the rows per
     * observation to recover the full filter list.
     */
    val configFields: Decoder[(Filter, Variant.Fields, ModeFields)] =
      (gnirs_filter                                *: // c_filter
       exposure_time_mode                          *: // the filter's science ETM
       int4_pos                                    *: // c_coadds
       GmosImagingService.Statements.variant_fields *:
       modeFields
      ).map: (filter, etm, coadds, variantFields, mode) =>
        (Filter(filter, etm, coadds), variantFields, mode)

    def select(
      oids: NonEmptyList[Observation.Id]
    ): AppliedFragment =
      sql"""
        SELECT
          v.c_observation_id,
          f.c_filter,
          sci.c_exposure_time_mode,
          sci.c_signal_to_noise_at,
          sci.c_signal_to_noise,
          sci.c_exposure_time,
          sci.c_exposure_count,
          f.c_coadds,
          #${ImagingStatements.variantColumns("v.")},
          v.c_camera,
          v.c_read_mode,
          v.c_well_depth_default,
          v.c_well_depth,
          v.c_acq_type,
          v.c_acq_coadds,
          v.c_acq_filter,
          v.c_acq_sky_offset_p,
          v.c_acq_sky_offset_q,
          acq.c_exposure_time_mode,
          acq.c_signal_to_noise_at,
          acq.c_signal_to_noise,
          acq.c_exposure_time,
          acq.c_exposure_count
        FROM v_gnirs_imaging v
        JOIN #${GnirsImagingService.FilterTableName} f
          ON f.c_observation_id = v.c_observation_id AND f.c_version = 'current'
        JOIN t_exposure_time_mode sci
          ON sci.c_exposure_time_mode_id = f.c_exposure_time_mode_id
        JOIN t_exposure_time_mode acq
          ON acq.c_observation_id = v.c_observation_id AND acq.c_role = 'acquisition'
        WHERE """(Void) |+| observationIdIn(oids, "v".some) |+|
      void" ORDER BY v.c_observation_id, f.c_filter"

    def insert(
      input: GnirsImagingInput.Create,
      which: NonEmptyList[Observation.Id]
    ): AppliedFragment =
      // The sky offset is stored only for an explicit FAINT acquisition type; the input
      // binding already rejects any other combination.
      val acqSkyOffset: Option[Offset] = input.acquisition.flatMap(_.skyOffset)

      val modeEntries =
        which.map: oid =>
          sql"""(
            $observation_id,
            (SELECT c_program_id FROM t_observation WHERE c_observation_id = $observation_id),
            $gnirs_camera,
            ${gnirs_read_mode.opt},
            ${gnirs_well_depth.opt},
            ${gnirs_acquisition_type.opt},
            $int4,
            ${gnirs_filter.opt},
            ${angle_µas.opt},
            ${angle_µas.opt},
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
            input.camera,
            input.explicitReadMode,
            input.explicitWellDepth,
            input.acquisition.flatMap(_.explicitAcqType.toOption),
            input.acquisition.flatMap(_.coadds).getOrElse(GnirsImagingInput.DefaultCoadds).value,
            input.acquisition.flatMap(_.explicitFilter.toOption),
            acqSkyOffset.map(_.p.toAngle),
            acqSkyOffset.map(_.q.toAngle),
            input.variant.variantType,
            ImagingVariantInput.order.getOption(input.variant).flatten.getOrElse(WavelengthOrder.Increasing),
            ImagingVariantInput.skyCount.getOption(input.variant).flatten.getOrElse(NonNegInt.MinValue),
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
          c_read_mode,
          c_well_depth,
          c_acq_type,
          c_acq_coadds,
          c_acq_filter,
          c_acq_sky_offset_p,
          c_acq_sky_offset_q,
          #${ImagingStatements.variantColumns()}
        ) VALUES
      """(Void) |+| modeEntries.intercalate(void", ")

    /**
     * Inserts the filter rows for one row version.  Unlike the other imaging
     * modes, GNIRS carries coadds per filter, so this doesn't use the shared
     * `ImagingStatements.insertFilters`.
     */
    def insertFilters(
      rows:    NonEmptyList[(Observation.Id, GnirsFilter, PosInt, ExposureTimeModeId)],
      version: ObservingModeRowVersion
    ): AppliedFragment =
      val insertInto: AppliedFragment =
        void"""
          INSERT INTO t_gnirs_imaging_filter (
            c_observation_id,
            c_filter,
            c_version,
            c_coadds,
            c_exposure_time_mode_id
          ) VALUES
        """

      val values =
        rows.map: (oid, filter, coadds, eid) =>
          sql"($observation_id, $gnirs_filter, $observing_mode_row_version, $int4_pos, $exposure_time_mode_id)"(
            oid, filter, version, coadds, eid
          )

      insertInto |+| values.intercalate(void", ")

    /**
     * Copies the filter rows to a cloned observation, remapping each to its
     * cloned exposure time mode row.  Carries the coadds that the shared
     * `ImagingStatements.cloneFiltersAndEtms` doesn't know about.
     */
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
        INSERT INTO t_gnirs_imaging_filter (
          c_observation_id,
          c_exposure_time_mode_id,
          c_filter,
          c_version,
          c_coadds,
          c_role
        )
        SELECT
          $observation_id,
          e.new_exposure_time_mode_id,
          f.c_filter,
          f.c_version,
          f.c_coadds,
          f.c_role
        FROM t_gnirs_imaging_filter f
        JOIN etm_map e ON f.c_exposure_time_mode_id = e.old_exposure_time_mode_id
        WHERE f.c_observation_id = $observation_id
      """.apply(etms.map(_._1), etms.map(_._2), newId, originalId)

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
      val upReadMode  = sql"c_read_mode  = ${gnirs_read_mode.opt}"
      val upWellDepth = sql"c_well_depth = ${gnirs_well_depth.opt}"

      // Acquisition inline (non-ETM) column updates
      val upAcqType    = sql"c_acq_type         = ${gnirs_acquisition_type.opt}"
      val upAcqCoadds  = sql"c_acq_coadds       = $int4_pos"
      val upAcqFilter  = sql"c_acq_filter       = ${gnirs_filter.opt}"
      val upAcqSkyOffP = sql"c_acq_sky_offset_p = ${int8.opt}"
      val upAcqSkyOffQ = sql"c_acq_sky_offset_q = ${int8.opt}"

      // The acquisition type and sky offset are coupled: input validation guarantees a
      // sky offset is present iff the explicit type is FAINT, so whenever the type is
      // (re)set we rewrite the offset columns too — the provided offset for FAINT, NULL
      // otherwise. When the type is left unchanged we touch neither.
      val acqUpdates: List[AppliedFragment] =
        input.acquisition.toList.flatMap: acq =>
          val typeAndOffset: List[AppliedFragment] =
            acq.explicitAcqType.toOptionOption match
              case Some(tOpt) =>
                List(
                  upAcqType(tOpt),
                  upAcqSkyOffP(acq.skyOffset.map(o => Angle.microarcseconds.get(o.p.toAngle))),
                  upAcqSkyOffQ(acq.skyOffset.map(o => Angle.microarcseconds.get(o.q.toAngle)))
                )
              case None       =>
                Nil
          List(
            acq.coadds.map(upAcqCoadds),
            acq.explicitFilter.toOptionOption.map(upAcqFilter)
          ).flatten ++ typeAndOffset

      List(
        input.camera.map(upCamera),
        input.explicitReadMode.toOptionOption.map(upReadMode),
        input.explicitWellDepth.toOptionOption.map(upWellDepth)
      ).flatten ++ acqUpdates

    def clone(
      originalId: Observation.Id,
      newId:      Observation.Id
    ): AppliedFragment =
      sql"""
        INSERT INTO #${GnirsImagingService.ModeTableName} (
          c_observation_id,
          c_program_id,
          c_camera,
          c_read_mode,
          c_well_depth,
          c_acq_type,
          c_acq_coadds,
          c_acq_filter,
          c_acq_sky_offset_p,
          c_acq_sky_offset_q,
          #${ImagingStatements.variantColumns()}
        )
        SELECT
          $observation_id,
          (SELECT c_program_id FROM t_observation WHERE c_observation_id = $observation_id),
          c_camera,
          c_read_mode,
          c_well_depth,
          c_acq_type,
          c_acq_coadds,
          c_acq_filter,
          c_acq_sky_offset_p,
          c_acq_sky_offset_q,
          #${ImagingStatements.variantColumns()}
        FROM #${GnirsImagingService.ModeTableName}
        WHERE c_observation_id = $observation_id
      """.apply(newId, newId, originalId)
