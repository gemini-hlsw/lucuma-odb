// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.ImagingVariantType
import lucuma.core.enums.WavelengthOrder
import lucuma.core.math.Offset
import lucuma.core.model.Observation
import lucuma.odb.data.ExposureTimeModeId
import lucuma.odb.data.ExposureTimeModeRole
import lucuma.odb.data.ObservingModeRowVersion
import lucuma.odb.graphql.input.ImagingVariantInput
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.implicits.*

/**
 * Common sql statems for f2/gmos imaging modes.
 */
object ImagingStatements:

  // Variant column names shared by the f2/gmos imaging mode tables
  private val VariantColumnNames: NonEmptyList[String] = NonEmptyList.of(
    "c_variant",
    "c_wavelength_order",
    "c_sky_count",
    "c_exposures_per_offset",
    "c_pre_imaging_off1_p",
    "c_pre_imaging_off1_q",
    "c_pre_imaging_off2_p",
    "c_pre_imaging_off2_q",
    "c_pre_imaging_off3_p",
    "c_pre_imaging_off3_q",
    "c_pre_imaging_off4_p",
    "c_pre_imaging_off4_q"
  )

  // The shared variant columns as a comma-separated SQL fragment
  def variantColumns(prefix: String = ""): String =
    VariantColumnNames.toList.map(prefix + _).mkString(",\n")

  def variantUpdates(
    input: ImagingVariantInput
  ): List[AppliedFragment] =
    val upVariant = sql"c_variant = $imaging_variant"

    // Sky count requires special handling because it is used by two different
    // modes.  If we are simply updating an existing mode, then we don't
    // change the sky count value unless it is explicitly set.  If we are
    // switching modes, then we reset the value if it is not explicitly set.
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
      val upOrder           = sql"c_wavelength_order = ${wavelength_order}"
      val upExposuresPerOffset = sql"c_exposures_per_offset = ${int4_pos}"
      input match
        case ImagingVariantInput.Grouped(order, _, skyCount, _, exposuresPerOffset) =>
          List(
            upVariant(ImagingVariantType.Grouped),
            upSkyCount(ImagingVariantType.Grouped, skyCount)
          ) ++ order.map(upOrder).toList ++ exposuresPerOffset.map(upExposuresPerOffset).toList
        case _                                                                   =>
          List(upOrder(WavelengthOrder.Increasing), upExposuresPerOffset(PosInt.MinValue))

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
    filterTableName: String,
    which:           NonEmptyList[Observation.Id]
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
              FROM #$filterTableName f
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
    filterTableName: String,
    filterCodec:     Codec[L],
    rows:            NonEmptyList[(Observation.Id, L, ExposureTimeModeId)],
    version:         ObservingModeRowVersion
  ): AppliedFragment =
    val insertInto: AppliedFragment =
      sql"""
        INSERT INTO #$filterTableName (
          c_observation_id,
          c_filter,
          c_version,
          c_exposure_time_mode_id
        ) VALUES
      """(Void)

    val filterEntries =
      rows.map: (oid, filter, eid) =>
        sql"""($observation_id, $filterCodec, $observing_mode_row_version, $exposure_time_mode_id)"""(oid, filter, version, eid)

    insertInto |+| filterEntries.intercalate(void", ")

  def cloneFiltersAndEtms(
    filterTableName: String,
    originalId:      Observation.Id,
    newId:           Observation.Id,
    etms:            List[(ExposureTimeModeId, ExposureTimeModeId)]
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
      INSERT INTO #$filterTableName (
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
      FROM #$filterTableName f
      JOIN etm_map e ON f.c_exposure_time_mode_id = e.old_exposure_time_mode_id
      WHERE f.c_observation_id = $observation_id
    """.apply(etms.map(_._1), etms.map(_._2), newId, originalId)
