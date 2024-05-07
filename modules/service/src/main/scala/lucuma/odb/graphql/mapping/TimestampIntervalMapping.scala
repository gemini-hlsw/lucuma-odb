// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import io.circe.syntax.*
import lucuma.core.util.TimestampInterval
import lucuma.odb.graphql.table.CallForProposalsView
import lucuma.odb.json.time.query.given

trait TimestampIntervalMapping[F[_]] extends CallForProposalsView[F] {

  private def timestampIntervalMapping(
    valueColumn: ColumnRef,
    idColumn: ColumnRef
  ): ObjectMapping = {
    val value = FieldRef[TimestampInterval]("value")
    ObjectMapping(
      tpe = TimestampIntervalType,
      fieldMappings = List(
        SqlField(s"synthetic_id", idColumn, key = true, hidden = true),
        SqlField("value", valueColumn, hidden = true),
        value.as("start", _.start),
        value.as("end", _.end),
        CursorFieldJson("duration", _.fieldAs[TimestampInterval]("value").map(_.boundedTimeSpan.asJson), List("value"))
      )

    )
  }

  lazy val TimestampIntervalMappings: List[TypeMapping] =
    SwitchMapping(
      TimestampIntervalType,
      List(
        CallForProposalsType / "active" -> timestampIntervalMapping(CallForProposalsView.Active, CallForProposalsView.Id)
      )
    )

}