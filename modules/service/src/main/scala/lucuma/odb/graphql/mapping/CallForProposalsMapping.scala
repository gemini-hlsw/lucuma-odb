// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import io.circe.syntax.*
import lucuma.core.util.TimestampInterval
import lucuma.odb.graphql.table.CallForProposalsTable

trait CallForProposalsMapping[F[_]] extends CallForProposalsTable[F] {

  lazy val CallForProposalsPartnerMapping: TypeMapping =
    ObjectMapping(
      tpe = CallForProposalsPartnerType,
      fieldMappings = List(
        SqlField("id",       CallForProposalsPartnerTable.CfpId, hidden = true, key = true),
        SqlField("partner",  CallForProposalsPartnerTable.Partner, key = true),
        SqlField("deadline", CallForProposalsPartnerTable.Deadline)
      )
    )

  lazy val CallForProposalsMapping: TypeMapping =
    ObjectMapping(
      tpe = CallForProposalsType,
      fieldMappings = List(
        SqlField("id",       CallForProposalsTable.Id, key = true),
        SqlField("status",   CallForProposalsTable.Status),
        SqlField("type",     CallForProposalsTable.Type),
        SqlField("semester", CallForProposalsTable.Semester),
        SqlObject("raLimitStart"),
        SqlObject("raLimitEnd"),
        SqlObject("decLimitStart"),
        SqlObject("decLimitEnd"),

        SqlField("_active", CallForProposalsTable.Active, hidden = true),

        CursorFieldJson(
          "activeStart",
          _.fieldAs[TimestampInterval]("_active").map(_.start.asJson),
          List("_active")
        ),

        CursorFieldJson(
          "activeEnd",
          _.fieldAs[TimestampInterval]("_active").map(_.end.asJson),
          List("_active")
        ),

        SqlObject("partners", Join(CallForProposalsTable.Id, CallForProposalsPartnerTable.CfpId)),

        // instruments?

        SqlField("existence", CallForProposalsTable.Existence)
      )
    )
}
