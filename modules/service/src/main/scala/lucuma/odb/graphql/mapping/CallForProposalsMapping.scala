// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.effect.Resource
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

trait CallForProposalsMapping[F[_]] extends CallForProposalsView[F]:

  def user: User
  def services: Resource[F, Services[F]]

  lazy val CallForProposalsPartnerMapping: TypeMapping =
    ObjectMapping(CallForProposalsPartnerType)(
      SqlField("id",                         CallForProposalsPartnerView.CfpId, hidden = true, key = true),
      SqlField("geminiPartner",              CallForProposalsPartnerView.GeminiPartner, key = true),
      SqlField("submissionDeadlineOverride", CallForProposalsPartnerView.DeadlineOverride),
      SqlField("submissionDeadline",         CallForProposalsPartnerView.Deadline)
    )

  lazy val GeminiCallPropertiesMapping: TypeMapping =
    ObjectMapping(GeminiCallPropertiesType)(
      SqlField("id",                 CallForProposalsView.gemini.Id, key = true, hidden = true),
      SqlField("type",               CallForProposalsView.gemini.Type),
      SqlObject("coordinateLimits"),
      SqlField("instruments",        CallForProposalsView.gemini.Instruments),
      SqlField("proprietaryMonths",  CallForProposalsView.gemini.Proprietary),
      SqlField("allowsNonPartnerPi", CallForProposalsView.gemini.AllowsNonPartner),
      SqlField("nonPartnerDeadline", CallForProposalsView.gemini.NonPartnerDeadline),
      SqlField("exchangePartners",   CallForProposalsView.gemini.ExchangePartners)
    )

  lazy val KeckCallPropertiesMapping: TypeMapping =
    ObjectMapping(KeckCallPropertiesType)(
      SqlField("id",          CallForProposalsView.keck.Id, key = true, hidden = true),
      SqlField("instruments", CallForProposalsView.keck.Instruments),
      SqlObject("coordinateLimits")
    )

  lazy val SubaruCallPropertiesMapping: TypeMapping =
    ObjectMapping(SubaruCallPropertiesType)(
      SqlField("id",          CallForProposalsView.subaru.Id, key = true, hidden = true),
      SqlField("type",        CallForProposalsView.subaru.Type),
      SqlField("instruments", CallForProposalsView.subaru.Instruments),
      SqlObject("coordinateLimits")
    )

  lazy val CallForProposalsMapping: TypeMapping =
    ObjectMapping(CallForProposalsType)(
      SqlField("id",                        CallForProposalsView.Id, key = true),
      SqlField("title",                     CallForProposalsView.Title),
      SqlField("semester",                  CallForProposalsView.Semester),
      SqlObject("active"),
      SqlObject("partners",                 Join(CallForProposalsView.Id, CallForProposalsPartnerView.CfpId)),
      SqlField("submissionDeadlineDefault", CallForProposalsView.DeadlineDefault),
      SqlField("existence",                 CallForProposalsView.Existence),
      SqlField("observatory",               CallForProposalsView.Observatory),
      SqlObject("gemini"),
      SqlObject("keck"),
      SqlObject("subaru"),
      SqlField("_isOpen",                   CallForProposalsView.IsOpen, hidden = true)
    )

  lazy val CallForProposalsMappings: List[TypeMapping] =
    List(
      CallForProposalsPartnerMapping,
      GeminiCallPropertiesMapping,
      KeckCallPropertiesMapping,
      SubaruCallPropertiesMapping,
      CallForProposalsMapping
    )

  lazy val CallForProposalsElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (CallForProposalsType, "partners", Nil) =>
      Elab.transformChild: child =>
        OrderBy(
          OrderSelections(
            List(
              OrderSelection[Partner](CallForProposalsPartnerType / "geminiPartner")
            )
          ),
          child
        )