// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import grackle.Path
import lucuma.odb.graphql.table.CallForProposalsView

import java.time.LocalDate

trait DateIntervalMapping[F[_]] extends CallForProposalsView[F] {

  private def dateIntervalMappingAtPath(
    path: Path,
    idColumn: ColumnRef,
    startColumn: ColumnRef,
    endColumn:   ColumnRef,
    inclusiveEnd: Boolean = false
  ): ObjectMapping =
    ObjectMapping(path)(
      SqlField(s"synthetic_id", idColumn, key = true, hidden = true),
      SqlField("start", startColumn),
      SqlField("_exclusive_end", endColumn, hidden = true),
      FieldRef[LocalDate]("_exclusive_end").as("end", date => if (inclusiveEnd) date.minusDays(1L) else date)
    )

  lazy val DateIntervalMappings: List[TypeMapping] =
    List(
      dateIntervalMappingAtPath(CallForProposalsType / "active", CallForProposalsView.Id, CallForProposalsView.ActiveStart, CallForProposalsView.ActiveEnd, inclusiveEnd = true)
    )

}