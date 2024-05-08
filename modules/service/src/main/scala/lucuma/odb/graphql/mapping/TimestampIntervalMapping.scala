// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Path
import io.circe.syntax.*
import lucuma.core.util.TimestampInterval
import lucuma.odb.graphql.table.CallForProposalsView
import lucuma.odb.json.time.query.given

trait TimestampIntervalMapping[F[_]] extends CallForProposalsView[F] {

  private def timestampIntervalMappingAtPath(
    path: Path,
    valueColumn: ColumnRef,
    idColumn: ColumnRef
  ): ObjectMapping =
    val value = FieldRef[TimestampInterval]("value")
    ObjectMapping(PathMatch(path))(
      SqlField(s"synthetic_id", idColumn, key = true, hidden = true),
      SqlField("value", valueColumn, hidden = true),
      value.as("start", _.start),
      value.as("end", _.end),
      CursorFieldJson("duration", _.fieldAs[TimestampInterval]("value").map(_.boundedTimeSpan.asJson), List("value"))
    )

  lazy val TimestampIntervalMappings: List[TypeMapping] =
    List(
      timestampIntervalMappingAtPath(CallForProposalsType / "active", CallForProposalsView.Active, CallForProposalsView.Id)
    )

}