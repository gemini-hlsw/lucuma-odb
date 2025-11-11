// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
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
    idColumns:     ColumnRef*
  ): ObjectMapping =
    ObjectMapping(path)(
      (
        idColumns.toList.map: ref =>
          SqlField(ref.column, ref, key = true, hidden = true)
      ) ++ List(
        SqlObject("offset"),
        SqlField("guiding", guidingColumn)
      )*
    )

  private def enumeratedTelescopeConfigMapping: ObjectMapping =
    ObjectMapping(TelescopeConfigType)(
      SqlField("observationId", EnumeratedOffsetView.ObservationId, key = true, hidden = true),
      SqlField("role",          EnumeratedOffsetView.OffsetGeneratorRole, key = true, hidden = true),
      SqlField("index",         EnumeratedOffsetView.Index, key = true, hidden = true),
      SqlObject("offset"),
      SqlField("guiding",       EnumeratedOffsetView.GuideState)
    )


  lazy val TelescopeConfigMappings: List[ObjectMapping] =
    List(
//      telescopeConfigMappingAtPath(EnumeratedOffsetGeneratorType / "values", EnumeratedOffsetTable.GuideState, EnumeratedOffsetTable.ObservationId, EnumeratedOffsetTable.OffsetGeneratorRole, EnumeratedOffsetTable.Index),
      enumeratedTelescopeConfigMapping,
      telescopeConfigMappingAtPath(StepRecordType / "telescopeConfig", StepRecordView.GuideState, StepRecordView.Id)
    )