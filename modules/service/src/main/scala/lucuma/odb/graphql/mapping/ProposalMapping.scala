// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.effect.Resource
import grackle.Query.EffectHandler
import grackle.skunk.SkunkMapping
import lucuma.core.model.Program
import lucuma.core.model.sequence.CategorizedTimeRange
import lucuma.core.util.CalculatedValue
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.graphql.table.CallForProposalsView
import lucuma.odb.graphql.table.PartnerSplitTable
import lucuma.odb.graphql.table.ProgramView
import lucuma.odb.graphql.table.ProposalReferenceView
import lucuma.odb.graphql.table.ProposalView
import lucuma.odb.json.calculatedValue.given
import lucuma.odb.json.time.query.given
import lucuma.odb.json.timeaccounting.given
import lucuma.odb.service.Services

import Services.Syntax.*

trait ProposalMapping[F[_]] extends PartnerSplitTable[F]
                               with CallForProposalsView[F]
                               with Predicates[F]
                               with ProgramView[F]
                               with ProposalReferenceView[F]
                               with ProposalView[F]
                               with KeyValueEffectHandler[F] {

  def services: Resource[F, Services[F]]

  lazy val ProposalMapping =
    ObjectMapping(ProposalType)(
      SqlField("program_id", ProposalView.ProgramId, key = true, hidden = true),
      SqlObject("reference", Join(ProposalView.ProgramId, ProposalReferenceView.Id)),
      SqlObject("call", Join(ProposalView.CallId, CallForProposalsView.Id)),
      SqlField("category", ProposalView.Category),
      SqlObject("explicitTimeRequest"),
      EffectField("timeRequest", timeRequestHandler, List("program_id")),
      EffectField("defaultTimeRequest", defaultTimeRequestHandler, List("program_id")),
      SqlObject("gemini"),
      SqlObject("keck"),
      SqlObject("subaru")
    )

  private lazy val timeRequestHandler: EffectHandler[F] =
    keyValueEffectHandler[Program.Id, Option[CalculatedValue[CategorizedTimeRange]]]("program_id"): pid =>
      services.useTransactionally:
        proposalService.timeRequest(pid)

  // The derived request on its own, which the effective one falls back to.  It
  // is the same figure as the program's `timeEstimateRange`, and keeps tracking
  // the observations even while an explicit request stands in for it.
  private lazy val defaultTimeRequestHandler: EffectHandler[F] =
    keyValueEffectHandler[Program.Id, Option[CalculatedValue[CategorizedTimeRange]]]("program_id"): pid =>
      services.useTransactionally:
        timeEstimateService.estimateProgramRange(pid)

}
