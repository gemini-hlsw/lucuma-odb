// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Path

import table.EnumeratedOffsetView
import table.StepRecordView

trait TelescopeConfigMapping[F[_]] extends EnumeratedOffsetView[F] with StepRecordView[F]:

  private def telescopeConfigMappingAtPath(
    path:          Path,
    guidingColumn: ColumnRef,
    purposeColumn: ColumnRef,
    idColumns:     (String, ColumnRef)*
  ): ObjectMapping =
    ObjectMapping(path)(
      (
        idColumns.toList.map: (name, ref) =>
          SqlField(name, ref, key = true, hidden = true)
      ) ++ List(
        SqlObject("offset"),
        SqlField("guiding", guidingColumn),
        SqlField("purpose", purposeColumn)
      )*
    )

  lazy val TelescopeConfigMappings: List[ObjectMapping] =
    List(
      telescopeConfigMappingAtPath(
        EnumeratedTelescopeConfigGeneratorType / "values",
        EnumeratedOffsetView.GuideState,
        // An enumerated position's purpose is that of its (object or sky) generator.
        EnumeratedOffsetView.OffsetGeneratorRole,
        "observationId" -> EnumeratedOffsetView.ObservationId,
        "role"          -> EnumeratedOffsetView.OffsetGeneratorRole,
        "index"         -> EnumeratedOffsetView.Index
      ),

      telescopeConfigMappingAtPath(
        StepRecordType / "telescopeConfig",
        StepRecordView.GuideState,
        StepRecordView.OffsetPurpose,
        "id" -> StepRecordView.Id
      )
    )