// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.parse.*
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import grackle.Result
import grackle.ResultT
import grackle.syntax.*
import lucuma.core.enums.Site
import lucuma.core.enums.WavelengthOrder
import lucuma.core.math.Offset
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.odb.data.ExposureTimeModeId
import lucuma.odb.data.ExposureTimeModeRole
import lucuma.odb.data.Nullable
import lucuma.odb.data.ObservingModeRowVersion
import lucuma.odb.data.TelescopeConfigGeneratorRole
import lucuma.odb.graphql.input.GmosImagingFilterInput
import lucuma.odb.graphql.input.GmosImagingInput
import lucuma.odb.graphql.input.GmosImagingVariantInput
import lucuma.odb.sequence.data.TelescopeConfigGenerator
import lucuma.odb.sequence.gmos.imaging.Config
import lucuma.odb.sequence.gmos.imaging.Filter
import lucuma.odb.sequence.gmos.imaging.Variant
import lucuma.odb.sequence.gmos.imaging.VariantType
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GmosCodecs.*
import skunk.*
import skunk.data.Arr
import skunk.data.Type
import skunk.implicits.*

import Services.Syntax.*

sealed trait GmosImagingService[F[_]]:

  def selectNorth(
    which: List[Observation.Id]
  ): F[Map[Observation.Id, Config.GmosNorth]]

  def selectSouth(
    which: List[Observation.Id]
  ): F[Map[Observation.Id, Config.GmosSouth]]

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

  val siteName: Site => String = {
    case Site.GN => "north"
    case Site.GS => "south"
  }

  def modeViewName(s: Site): String =
    s"v_gmos_${siteName(s)}_imaging"

  def modeTableName(s: Site): String =
    s"t_gmos_${siteName(s)}_imaging"

  def filterTableName(s: Site): String =
    s"t_gmos_${siteName(s)}_imaging_filter"


  def instantiate[F[_]: Concurrent](using Services[F]): GmosImagingService[F] =
    new GmosImagingService[F]:

      override def selectNorth(
        which: List[Observation.Id]
      ): F[Map[Observation.Id, Config.GmosNorth]] =
        select(Site.GN, which, gmos_north_filter).map: m =>
          m.view.mapValues((fs, v, c) => Config.GmosNorth(v, fs, c)).toMap

      override def selectSouth(
        which: List[Observation.Id]
      ): F[Map[Observation.Id, Config.GmosSouth]] =
        select(Site.GS, which, gmos_south_filter).map: m =>
          m.view.mapValues((fs, v, c) => Config.GmosSouth(v, fs, c)).toMap

      private def select[L](
        site:   Site,
        which:  List[Observation.Id],
        filter: Codec[L]
      ): F[Map[Observation.Id, (NonEmptyList[Filter[L]], Variant, Config.Common)]] =
        NonEmptyList
          .fromList(which)
          .fold(Map.empty[Observation.Id, (NonEmptyList[Filter[L]], Variant, Config.Common)].pure[F]): oids =>

            val decoder: Decoder[(NonEmptyList[Filter[L]], Variant.Fields, Config.Common)] =
              Statements.filter_list(filter) *: Statements.variant_fields *: Statements.common

            val precursorMap =
              val af = Statements.select(modeTableName(site), filterTableName(site), oids)
              session
                .prepareR(af.fragment.query(observation_id *: decoder)).use: pq =>
                  pq.stream(af.argument, chunkSize = 1024)
                    .compile
                    .toList
                    .map(_.map((oid, fs, variantFields, common) => oid -> (fs, variantFields, common)).toMap)

            for
              c <- precursorMap
              o <- services.telescopeConfigGeneratorService.select(oids, TelescopeConfigGeneratorRole.Object)
              s <- services.telescopeConfigGeneratorService.select(oids, TelescopeConfigGeneratorRole.Sky)
            yield c.view.map { case (oid, (fs, variantFields, common)) =>
              val og = o.getOrElse(oid, TelescopeConfigGenerator.NoGenerator)
              val sg = s.getOrElse(oid, TelescopeConfigGenerator.NoGenerator)
              oid -> (fs, variantFields.toVariant(og, sg), common)
            }.toMap

      override def insertNorth(
        input:  GmosImagingInput.Create.North,
        reqEtm: Option[ExposureTimeMode],
        which:  List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        insert(Site.GN, gmos_north_filter, input, reqEtm, which)

      override def insertSouth(
        input:  GmosImagingInput.Create.South,
        reqEtm: Option[ExposureTimeMode],
        which:  List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        insert(Site.GS, gmos_south_filter, input, reqEtm, which)

      private def insert[L](
        site:        Site,
        filterCodec: Codec[L],
        input:       GmosImagingInput.Create[GmosImagingFilterInput[L]],
        reqEtm:      Option[ExposureTimeMode],
        which:       List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        val modeName = s"GMOS ${siteName(site).capitalize} Imaging"
        NonEmptyList
          .fromList(which)
          .fold(ResultT.unit[F]): oids =>
            for
              _   <- ResultT.liftF(session.exec(Statements.insert(modeTableName(site), input, oids)))

              // Resolve the exposure time modes for acquisition and science
              r   <- ResultT(services.exposureTimeModeService.resolve(modeName, none, input.filters.map(f => (f.filter, f.exposureTimeMode)), reqEtm, which))

              // Insert the acquisition and science filters (initial / immutable version)
              ids <- ResultT.liftF(services.exposureTimeModeService.insertResolvedAcquisitionAndScience(r))
              ini  = stripAcquisition(ids)
              _   <- ResultT.liftF(insertFilters(ini, site, filterCodec, ObservingModeRowVersion.Initial))

              // Insert the science filters (current / mutable version)
              cur <- ResultT.liftF(services.exposureTimeModeService.insertResolvedScienceOnly(stripAcquisition(r)))
              _   <- ResultT.liftF(insertFilters(cur, site, filterCodec, ObservingModeRowVersion.Current))

              // Insert the offset generators
              _  <- ResultT.liftF:
                      Variant.offsets.getOption(input.variant).traverse_ : og =>
                        services.telescopeConfigGeneratorService.insert(oids, og, TelescopeConfigGeneratorRole.Object)

              _  <- ResultT.liftF:
                      Variant.skyOffsets.getOption(input.variant).traverse_ : og =>
                        services.telescopeConfigGeneratorService.insert(oids, og, TelescopeConfigGeneratorRole.Sky)
            yield ()
          .value

      private def stripAcquisition[L, E](
        m: Map[Observation.Id, (E, NonEmptyList[(L, E)])]
      ): Map[Observation.Id, NonEmptyList[(L, E)]] =
        m.view.mapValues(_._2).toMap

      private def insertFilters[L](
        etms:        Map[Observation.Id, NonEmptyList[(L, ExposureTimeModeId)]],
        site:        Site,
        filterCodec: Codec[L],
        version:     ObservingModeRowVersion
      ): F[Unit] =
        NonEmptyList
          .fromList:
            etms.toList.flatMap: (oid, fs) =>
              fs.toList.map: (filter, eid) =>
                (oid, filter, eid)
          .traverse_ : rs =>
            session.exec(Statements.insertFilters(filterTableName(site), filterCodec, rs, version))

      override def deleteNorth(
        which: List[Observation.Id]
      )(using Transaction[F]): F[Unit] =
        delete(Site.GN, which)

      override def deleteSouth(
        which: List[Observation.Id]
      )(using Transaction[F]): F[Unit] =
        delete(Site.GS, which)

      private def delete(
        site:  Site,
        which: List[Observation.Id]
      )(using Transaction[F]): F[Unit] =
        services
          .exposureTimeModeService
          .deleteMany(which, ExposureTimeModeRole.Acquisition, ExposureTimeModeRole.Science)
          .productR:
            session.exec(Statements.delete(modeTableName(site), which))

      override def updateNorth(
        SET: GmosImagingInput.Edit.North,
        which: List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        update(Site.GN, gmos_north_filter, SET, none, which)

      override def updateSouth(
        SET: GmosImagingInput.Edit.South,
        which: List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        update(Site.GS, gmos_south_filter, SET, none, which)

      private def update[L](
        site:           Site,
        filterCodec:    Codec[L],
        edit:           GmosImagingInput.Edit[GmosImagingFilterInput[L]],
        newRequirement: Option[ExposureTimeMode],
        which:          List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        val modeName = s"GMOS ${siteName(site)} Imaging"

        NonEmptyList.fromList(which).fold(().success.pure): oids =>
          val modeUpdates =
            NonEmptyList
              .fromList(
                Statements.commonUpdates(edit.common) ++
                edit.variant.toList.flatMap(Statements.variantUpdates)
               )
              .traverse_ : us =>
                session.exec:
                  sql"UPDATE #${modeTableName(site)} SET "(Void) |+|
                    us.intercalate(void", ")                     |+|
                    void" WHERE "                                |+|
                    observationIdIn(oids)

          val filterUpdates =
            edit.filters.fold(ResultT.unit): fs =>
              for
                _ <- ResultT.liftF(session.exec(Statements.deleteCurrentFiltersAndEtms(filterTableName(site), oids)))

                // Resolve the exposure time modes for acquisition and science
                r   <- ResultT(services.exposureTimeModeService.resolve(modeName, none, fs.map(f => (f.filter, f.exposureTimeMode)), newRequirement, which))

                // Insert the acquisition and science filters (current / mutable version)
                ids <- ResultT.liftF(services.exposureTimeModeService.insertResolvedAcquisitionAndScience(r))
                cur  = stripAcquisition(ids)
                _   <- ResultT.liftF(insertFilters(cur, site, filterCodec, ObservingModeRowVersion.Current))
              yield ()

          def updateOffsetForRole(
            input:   Nullable[TelescopeConfigGenerator],
            variant: VariantType,
            role:    TelescopeConfigGeneratorRole
          ): F[Unit] =
            input.toOptionOption.fold(
              // the offset generator field was Absent, which means we should
              // default it to no generator when switching variants.
              session.exec(
                Statements.deleteOffsetGeneratorWhenNotMatchingVariant(
                  modeTableName(site),
                  oids,
                  variant,
                  role
                )
              )
            ): in =>
              services.telescopeConfigGeneratorService.replace(oids, in, role)

          val offsetUpdates =
            edit.variant.fold(().pure[F]): v =>
              val (o, s) = v match
                case GmosImagingVariantInput.Grouped(_, offsets, _, skyOffsets)  => (offsets, skyOffsets)
                case GmosImagingVariantInput.Interleaved(offsets, _, skyOffsets) => (offsets, skyOffsets)
                case _                                                           => (Nullable.Null, Nullable.Null)

              updateOffsetForRole(o, v.variantType, TelescopeConfigGeneratorRole.Object) *>
              updateOffsetForRole(s, v.variantType, TelescopeConfigGeneratorRole.Sky)

          (for
            _ <- ResultT.liftF(offsetUpdates)
            _ <- ResultT.liftF(modeUpdates)
            _ <- filterUpdates
          yield ()).value

      override def cloneNorth(
        observationId:    Observation.Id,
        newObservationId: Observation.Id,
        etms:             List[(ExposureTimeModeId, ExposureTimeModeId)]
      )(using Transaction[F]): F[Unit] =
        clone(Site.GN, observationId, newObservationId, etms)

      override def cloneSouth(
        observationId:    Observation.Id,
        newObservationId: Observation.Id,
        etms:             List[(ExposureTimeModeId, ExposureTimeModeId)]
      )(using Transaction[F]): F[Unit] =
        clone(Site.GS, observationId, newObservationId, etms)

      private def clone(
        site:             Site,
        observationId:    Observation.Id,
        newObservationId: Observation.Id,
        etms:             List[(ExposureTimeModeId, ExposureTimeModeId)]
      )(using Services[F]): F[Unit] =
        session.exec(Statements.clone(modeTableName(site), observationId, newObservationId))                       *>
        session.exec(Statements.cloneFiltersAndEtms(filterTableName(site), observationId, newObservationId, etms)) *>
        services.telescopeConfigGeneratorService.clone(observationId, newObservationId)

  object Statements:

    // Creates the codec for a single filter / ETM pair.  This corresponds to
    // the s_filter_exposure_time_mode composite type ("s" for struct).
    def filter[L](filter_codec: Codec[L]): Codec[Filter[L]] =
      (filter_codec       *:
       exposure_time_mode
      ).to[Filter[L]]

    // Decoder for a list of fiter / ETM pairs
    def filter_list[L](filter_codec: Codec[L]): Decoder[NonEmptyList[Filter[L]]] =
      val fieldText: Parser0[String] =
        Parser.charsWhile0(c => c =!= ',' && c =!= ')').string

      val field: Parser0[Option[String]] =
        fieldText.map:
          case ""     => none
          case "null" => none
          case s      => s.some

      val fields: Parser0[List[Option[String]]] =
        (field ~ (Parser.char(',') *> field).rep0).map: (h, t) =>
          h :: t

      val row: Parser[List[Option[String]]] =
        fields.with1.between(Parser.char('('), Parser.char(')'))

      // s_filter_exposure_time_mode codec
      val codec = filter(filter_codec)

      def encode(a: Filter[L]): String =
        codec.encode(a).map(_.getOrElse("")).mkString("(", ",", ")")

      def decode(s: String): Either[String, Filter[L]] =
        row.parseAll(s) match
          case Left(err)     => s"Could not parse the filter / exposure time mode row: ${err.toString}".asLeft
          case Right(fields) => codec.decode(0, fields).leftMap(e => s"Filter / exposure time  mode row decoder failure: ${e.message}")

      // I just need the decoder, but want to use `Codec.array`.  The encoder
      // might work as well, but it hasn't been tested.  We'll just make the
      // return type `Decoder`.
      Codec
        .array(encode, decode, Type("_s_filter_exposure_time_mode", List(Type("s_filter_exposure_time_mode", List(Type("d_tag"), Type("e_exp_time_mode"), Type("d_wavelength_pm"), Type("numeric"), Type("interval"), Type("int4"))))))
        .eimap(arr => NonEmptyList.fromList(arr.toList).toRight("At least one filter entry expected"))(Arr.fromFoldable)

    val variant_fields: Decoder[Variant.Fields] =
      (gmos_imaging_variant *:
       wavelength_order     *:
       int4_nonneg          *:
       offset               *:
       offset               *:
       offset               *:
       offset
      ).to[Variant.Fields]

    val common: Decoder[Config.Common] =
      (
        gmos_binning              *:
        gmos_binning.opt          *:
        gmos_amp_read_mode.opt    *:
        gmos_amp_gain.opt         *:
        gmos_roi.opt
      ).to[Config.Common]

    def select(
      table:       String,
      filterTable: String,
      oids:        NonEmptyList[Observation.Id]
    ): AppliedFragment =
      sql"""
        WITH selected_observations AS (
          SELECT
            t.c_observation_id
          FROM #$table t
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
          FROM #$filterTable f
          JOIN t_exposure_time_mode e ON e.c_exposure_time_mode_id = f.c_exposure_time_mode_id
          JOIN selected_observations s ON s.c_observation_id  = f.c_observation_id
          WHERE f.c_version = 'current'
          GROUP BY f.c_observation_id
        )
        SELECT
          t.c_observation_id,
          f.c_filters,
          t.c_variant,
          t.c_wavelength_order,
          t.c_sky_count,
          t.c_pre_imaging_off1_p,
          t.c_pre_imaging_off1_q,
          t.c_pre_imaging_off2_p,
          t.c_pre_imaging_off2_q,
          t.c_pre_imaging_off3_p,
          t.c_pre_imaging_off3_q,
          t.c_pre_imaging_off4_p,
          t.c_pre_imaging_off4_q,
          t.c_bin_default,
          t.c_bin,
          t.c_amp_read_mode,
          t.c_amp_gain,
          t.c_roi
        FROM #$table t
        JOIN aggregated_filters f ON f.c_observation_id = t.c_observation_id;
      """(Void)

    def deleteOffsetGeneratorWhenNotMatchingVariant(
      tableName: String,
      which:     NonEmptyList[Observation.Id],
      variant:   VariantType,
      role:      TelescopeConfigGeneratorRole
    ): AppliedFragment =
      sql"""
        DELETE FROM t_offset_generator AS og
        USING #$tableName AS img
        WHERE og.c_observation_id = img.c_observation_id
          AND og.c_role = $offset_generator_role
          AND og.c_observation_id IN ${observation_id.list(which.length).values}
          AND img.c_variant <> $gmos_imaging_variant
      """.apply(role, which.toList, variant)

    def insert[L](
      modeTable: String,
      input:     GmosImagingInput.Create[L],
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
            $gmos_imaging_variant,
            $wavelength_order,
            $int4_nonneg,
            $offset,
            $offset,
            $offset,
            $offset
          )"""(
            oid,
            input.common.explicitBin,
            input.common.explicitAmpReadMode,
            input.common.explicitAmpGain,
            input.common.explicitRoi,
            input.variant.variantType,
            input.variant.grouped.map(_.order).getOrElse(Variant.Grouped.Default.order),
            input.variant.grouped.map(_.skyCount).orElse(input.variant.interleaved.map(_.skyCount)).getOrElse(NonNegInt.MinValue),
            input.variant.preImaging.map(_.offset1).getOrElse(Offset.Zero),
            input.variant.preImaging.map(_.offset2).getOrElse(Offset.Zero),
            input.variant.preImaging.map(_.offset3).getOrElse(Offset.Zero),
            input.variant.preImaging.map(_.offset4).getOrElse(Offset.Zero)
          )

      sql"""
        INSERT INTO #$modeTable (
          c_observation_id,
          c_bin,
          c_amp_read_mode,
          c_amp_gain,
          c_roi,
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
          c_roi,
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
          """.apply(Void) |+| sql"$observation_id".apply(newId) |+| sql""",
          c_bin,
          c_amp_read_mode,
          c_amp_gain,
          c_roi,
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

    def variantUpdates(
      input: GmosImagingVariantInput
    ): List[AppliedFragment] =
      val upVariant  = sql"c_variant   = $gmos_imaging_variant"

      // Sky count requires special handling because it is used by two different
      // modes.  If we are simply updating an existing mode, then we don't
      // change the sky count value unless it is explicitly set.  If we are
      // switching modes, then we reset the value if it is not explicitly set.
      def upSkyCount(variant: VariantType, skyCount: Option[NonNegInt]): AppliedFragment =
        (variant, skyCount) match
          case (VariantType.PreImaging, _) =>
            sql"c_sky_count = $int4_nonneg"(NonNegInt.MinValue)

          case (v, None)                   =>
            sql"""
              c_sky_count =
                CASE
                  WHEN c_variant = $gmos_imaging_variant THEN c_sky_count
                  ELSE $int4_nonneg
                END
            """.apply(v, NonNegInt.MinValue)

          case (_, Some(sc))               =>
            sql"c_sky_count = $int4_nonneg"(sc)

      def grouped: List[AppliedFragment] =
        val upOrder = sql"c_wavelength_order = ${wavelength_order}"

        input match
          case GmosImagingVariantInput.Grouped(order, _, skyCount, _) =>
            List(
              upVariant(VariantType.Grouped),
              upSkyCount(VariantType.Grouped, skyCount)
            ) ++ order.map(upOrder).toList
          case _                                                         =>
            List(upOrder(Variant.Grouped.Default.order))

      def interleaved: List[AppliedFragment] =
        input match
          case GmosImagingVariantInput.Interleaved(_, skyCount, _) =>
            List(
              upVariant(VariantType.Interleaved),
              upSkyCount(VariantType.Interleaved, skyCount)
            )
          case _                                      =>
            Nil

      def preImaging: List[AppliedFragment] =
        val upPre1p = sql"c_pre_imaging_off1_p = ${angle_µas}"
        val upPre1q = sql"c_pre_imaging_off1_q = ${angle_µas}"
        val upPre2p = sql"c_pre_imaging_off2_p = ${angle_µas}"
        val upPre2q = sql"c_pre_imaging_off2_q = ${angle_µas}"
        val upPre3p = sql"c_pre_imaging_off3_p = ${angle_µas}"
        val upPre3q = sql"c_pre_imaging_off3_q = ${angle_µas}"
        val upPre4p = sql"c_pre_imaging_off4_p = ${angle_µas}"
        val upPre4q = sql"c_pre_imaging_off4_q = ${angle_µas}"

        input match
          case GmosImagingVariantInput.PreImaging(o1, o2, o3, o4) =>
            upSkyCount(VariantType.PreImaging, None) :: List(
              upVariant(VariantType.PreImaging).some,
              o1.map(o => upPre1p(o.p.toAngle)),
              o1.map(o => upPre1q(o.q.toAngle)),
              o2.map(o => upPre2p(o.p.toAngle)),
              o2.map(o => upPre2q(o.q.toAngle)),
              o3.map(o => upPre3p(o.p.toAngle)),
              o3.map(o => upPre3q(o.q.toAngle)),
              o4.map(o => upPre4p(o.p.toAngle)),
              o4.map(o => upPre4q(o.q.toAngle))
            ).flatten
          case _                                                     =>
            val zero = Variant.PreImaging.Default
            List(
              upPre1p(zero.offset1.p.toAngle),
              upPre1q(zero.offset1.q.toAngle),
              upPre2p(zero.offset2.p.toAngle),
              upPre2q(zero.offset2.q.toAngle),
              upPre3p(zero.offset3.p.toAngle),
              upPre3q(zero.offset3.q.toAngle),
              upPre4p(zero.offset4.p.toAngle),
              upPre4q(zero.offset4.q.toAngle)
            )

      grouped ++ interleaved ++ preImaging


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