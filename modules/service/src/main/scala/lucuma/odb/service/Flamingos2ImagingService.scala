// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import grackle.Result
import grackle.ResultT
import grackle.syntax.*
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.ImagingVariantType
import lucuma.core.enums.WavelengthOrder
import lucuma.core.math.Offset
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.odb.data.ExposureTimeModeId
import lucuma.odb.data.ExposureTimeModeRole
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

  def update(
    SET:   Flamingos2ImagingInput.Edit,
    which: List[Observation.Id]
  )(using Transaction[F]): F[Result[Unit]]

  def delete(
    which: List[Observation.Id]
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

      override def update(
        SET:   Flamingos2ImagingInput.Edit,
        which: List[Observation.Id]
      )(using Transaction[F]): F[Result[Unit]] =
        NonEmptyList.fromList(which).fold(().success.pure): oids =>

          val modeUpdates =
            NonEmptyList
              .fromList(
                Statements.commonUpdates(SET) ++
                SET.variant.toList.flatMap(Statements.variantUpdates)
              )
              .traverse_ : us =>
                session.exec:
                  sql"UPDATE #${Flamingos2ImagingService.ModeTableName} SET "(Void) |+|
                    us.intercalate(void", ")                                       |+|
                    void" WHERE "                                                  |+|
                    observationIdIn(oids)

          // Replace the current filters and their ETMs
          val filterUpdates =
            SET.filters.fold(ResultT.unit): fs =>
              for
                _   <- ResultT.liftF(session.exec(Statements.deleteCurrentFiltersAndEtms(oids)))
                // Insert the acquisition and science filters (current / mutable version)
                r   <- ResultT(services.exposureTimeModeService.resolve("Flamingos2 Imaging", none, fs.map(f => (f.filter, f.exposureTimeMode)), none, which))
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
                Flamingos2ImagingService.ModeTableName,
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
          yield ()).value

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

    def commonUpdates(
      input: Flamingos2ImagingInput.Edit
    ): List[AppliedFragment] =
      val upReadMode    = sql"c_read_mode    = ${flamingos_2_read_mode.opt}"
      val upReads       = sql"c_reads        = ${flamingos_2_reads.opt}"
      val upDecker      = sql"c_decker       = ${flamingos_2_decker.opt}"
      val upReadoutMode = sql"c_readout_mode = ${flamingos_2_readout_mode.opt}"
      List(
        input.explicitReadMode.toOptionOption.map(upReadMode),
        input.explicitReads.toOptionOption.map(upReads),
        input.explicitDecker.toOptionOption.map(upDecker),
        input.explicitReadoutMode.toOptionOption.map(upReadoutMode)
      ).flatten

    def variantUpdates(
      input: ImagingVariantInput
    ): List[AppliedFragment] =
      val upVariant = sql"c_variant = $imaging_variant"

      def upSkyCount(variant: ImagingVariantType, skyCount: Option[NonNegInt]): AppliedFragment =
        (variant, skyCount) match
          case (ImagingVariantType.PreImaging, _) =>
            sql"c_sky_count = $int4_nonneg"(NonNegInt.MinValue)
          case (v, None)                          =>
            sql"""
              c_sky_count =
                CASE
                  WHEN c_variant = $imaging_variant THEN c_sky_count
                  ELSE $int4_nonneg
                END
            """.apply(v, NonNegInt.MinValue)
          case (_, Some(sc))                      =>
            sql"c_sky_count = $int4_nonneg"(sc)

      def grouped: List[AppliedFragment] =
        val upOrder = sql"c_wavelength_order = ${wavelength_order}"
        input match
          case ImagingVariantInput.Grouped(order, _, skyCount, _) =>
            List(
              upVariant(ImagingVariantType.Grouped),
              upSkyCount(ImagingVariantType.Grouped, skyCount)
            ) ++ order.map(upOrder).toList
          case _                                                  =>
            List(upOrder(WavelengthOrder.Increasing))

      def interleaved: List[AppliedFragment] =
        input match
          case ImagingVariantInput.Interleaved(_, skyCount, _) =>
            List(
              upVariant(ImagingVariantType.Interleaved),
              upSkyCount(ImagingVariantType.Interleaved, skyCount)
            )
          case _                                               =>
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
          case ImagingVariantInput.PreImaging(o1, o2, o3, o4) =>
            upSkyCount(ImagingVariantType.PreImaging, None) :: List(
              upVariant(ImagingVariantType.PreImaging).some,
              o1.map(o => upPre1p(o.p.toAngle)),
              o1.map(o => upPre1q(o.q.toAngle)),
              o2.map(o => upPre2p(o.p.toAngle)),
              o2.map(o => upPre2q(o.q.toAngle)),
              o3.map(o => upPre3p(o.p.toAngle)),
              o3.map(o => upPre3q(o.q.toAngle)),
              o4.map(o => upPre4p(o.p.toAngle)),
              o4.map(o => upPre4q(o.q.toAngle))
            ).flatten
          case _                                              =>
            List(
              upPre1p(Offset.Zero.p.toAngle),
              upPre1q(Offset.Zero.q.toAngle),
              upPre2p(Offset.Zero.p.toAngle),
              upPre2q(Offset.Zero.q.toAngle),
              upPre3p(Offset.Zero.p.toAngle),
              upPre3q(Offset.Zero.q.toAngle),
              upPre4p(Offset.Zero.p.toAngle),
              upPre4q(Offset.Zero.q.toAngle)
            )

      grouped ++ interleaved ++ preImaging

    def deleteCurrentFiltersAndEtms(
      which: NonEmptyList[Observation.Id]
    ): AppliedFragment =
      // Deleting the exposure-time-mode rows cascades to the filter rows.
      sql"""
        DELETE FROM t_exposure_time_mode m
          WHERE m.c_observation_id IN ${observation_id.list(which.length).values}
          AND (
            m.c_role = $exposure_time_mode_role OR
            (
              m.c_role = $exposure_time_mode_role AND
              EXISTS (
                SELECT 1
                FROM #${Flamingos2ImagingService.FilterTableName} f
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
