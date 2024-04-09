// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import io.circe.syntax.*
import lucuma.core.util.TimestampInterval
import lucuma.odb.graphql.table.CallForProposalsView

trait CallForProposalsMapping[F[_]] extends CallForProposalsView[F] {

  lazy val CallForProposalsPartnerMapping: TypeMapping =
    ObjectMapping(
      tpe = CallForProposalsPartnerType,
      fieldMappings = List(
        SqlField("id",                 CallForProposalsPartnerTable.CfpId, hidden = true, key = true),
        SqlField("partner",            CallForProposalsPartnerTable.Partner, key = true),
        SqlField("submissionDeadline", CallForProposalsPartnerTable.Deadline)
      )
    )

  lazy val CallForProposalsMapping: TypeMapping =
    ObjectMapping(
      tpe = CallForProposalsType,
      fieldMappings = List(
        SqlField("id",       CallForProposalsView.Id, key = true),
        SqlField("type",     CallForProposalsView.Type),
        SqlField("semester", CallForProposalsView.Semester),
        SqlObject("raLimitStart"),
        SqlObject("raLimitEnd"),
        SqlObject("decLimitStart"),
        SqlObject("decLimitEnd"),

        SqlField("_active", CallForProposalsView.Active, hidden = true),

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

        SqlObject("partners", Join(CallForProposalsView.Id, CallForProposalsPartnerTable.CfpId)),

        SqlField("instruments", CallForProposalsView.Instruments),

        SqlField("existence", CallForProposalsView.Existence)
      )
    )
}
