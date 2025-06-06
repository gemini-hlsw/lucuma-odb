// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import grackle.Path
import lucuma.odb.graphql.table.CallForProposalsView
import lucuma.odb.graphql.table.ProgramTable

trait DateIntervalMapping[F[_]] extends CallForProposalsView[F]
                                   with ProgramTable[F]:

  private def dateIntervalMappingAtPath(
    path:        Path,
    startColumn: ColumnRef,
    endColumn:   ColumnRef,
    idColumn:    ColumnRef
  ): ObjectMapping =
    ObjectMapping(path)(
      SqlField(s"synthetic_id", idColumn, key = true, hidden = true),
      SqlField("start", startColumn),
      SqlField("end", endColumn)
    )

  lazy val DateIntervalMappings: List[TypeMapping] =
    List(
      dateIntervalMappingAtPath(CallForProposalsType / "active", CallForProposalsView.ActiveStart, CallForProposalsView.ActiveEnd, CallForProposalsView.Id),
      dateIntervalMappingAtPath(ProgramType / "active", ProgramTable.ActiveStart, ProgramTable.ActiveEnd, ProgramTable.Id)
    )