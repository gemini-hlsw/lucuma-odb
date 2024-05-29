// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.effect.Resource
import grackle.Query
import grackle.Query.Binding
import grackle.Query.OrderBy
import grackle.Query.OrderSelection
import grackle.Query.OrderSelections
import grackle.QueryCompiler.Elab
import grackle.TypeRef
import lucuma.core.enums.Partner
import lucuma.core.model.User
import lucuma.odb.graphql.table.CallForProposalsView
import lucuma.odb.service.Services

trait CallForProposalsMapping[F[_]] extends CallForProposalsView[F] {

  def user: User
  def services: Resource[F, Services[F]]

  lazy val CallForProposalsPartnerMapping: TypeMapping =
    ObjectMapping(CallForProposalsPartnerType)(
      SqlField("id",                         CallForProposalsPartnerView.CfpId, hidden = true, key = true),
      SqlField("partner",                    CallForProposalsPartnerView.Partner, key = true),
      SqlField("submissionDeadlineOverride", CallForProposalsPartnerView.DeadlineOverride),
      SqlField("submissionDeadline",         CallForProposalsPartnerView.Deadline)
    )

  lazy val CallForProposalsMapping: TypeMapping =
    ObjectMapping(CallForProposalsType)(
      SqlField("id",       CallForProposalsView.Id, key = true),
      SqlField("title",    CallForProposalsView.Title),
      SqlField("type",     CallForProposalsView.Type),
      SqlField("semester", CallForProposalsView.Semester),
      SqlObject("raLimitStart"),
      SqlObject("raLimitEnd"),
      SqlObject("decLimitStart"),
      SqlObject("decLimitEnd"),
      SqlField("submissionDeadlineDefault", CallForProposalsView.DeadlineDefault),
      SqlObject("active"),
      SqlObject("partners",   Join(CallForProposalsView.Id, CallForProposalsPartnerView.CfpId)),
      SqlField("instruments", CallForProposalsView.Instruments),
      SqlField("existence",   CallForProposalsView.Existence),
      SqlField("_isOpen",     CallForProposalsView.IsOpen, hidden = true)
    )

  lazy val CallForProposalsElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] = {
    case (CallForProposalsType, "partners", Nil) =>
      Elab.transformChild { child =>
        OrderBy(
          OrderSelections(
            List(
              OrderSelection[Partner](CallForProposalsPartnerType / "partner")
            )
          ),
          child
        )
      }
  }

}
