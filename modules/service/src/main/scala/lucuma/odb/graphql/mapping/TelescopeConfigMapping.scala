// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Path

import table.EnumeratedOffsetView
import table.StepView

trait TelescopeConfigMapping[F[_]] extends EnumeratedOffsetView[F] with StepView[F]:

  private def telescopeConfigMappingAtPath(
    path:          Path,
    guidingColumn: ColumnRef,
    idColumns:     (String, ColumnRef)*
  ): ObjectMapping =
    ObjectMapping(path)(
      (
        idColumns.toList.map: (name, ref) =>
          SqlField(name, ref, key = true, hidden = true)
      ) ++ List(
        SqlObject("offset"),
        SqlField("guiding", guidingColumn)
      )*
    )

  lazy val TelescopeConfigMappings: List[ObjectMapping] =
    List(
      telescopeConfigMappingAtPath(
        EnumeratedTelescopeConfigGeneratorType / "values",
        EnumeratedOffsetView.GuideState,
        "observationId" -> EnumeratedOffsetView.ObservationId,
        "role"          -> EnumeratedOffsetView.OffsetGeneratorRole,
        "index"         -> EnumeratedOffsetView.Index
      ),

      telescopeConfigMappingAtPath(
        StepRecordType / "telescopeConfig",
        StepView.GuideState,
        "id" -> StepView.Id
      )
    )