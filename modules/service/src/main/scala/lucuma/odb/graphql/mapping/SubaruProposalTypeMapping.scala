// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import eu.timepit.refined.cats.*
import grackle.Query.Binding
import grackle.Query.OrderBy
import grackle.Query.OrderSelection
import grackle.Query.OrderSelections
import grackle.QueryCompiler.Elab
import grackle.TypeRef
import lucuma.core.enums.Partner
import lucuma.core.model.IntPercent
import lucuma.odb.graphql.table.PartnerSplitTable
import lucuma.odb.graphql.table.ProposalView

trait SubaruProposalTypeMapping[F[_]] extends BaseMapping[F]
                                         with PartnerSplitTable[F]
                                         with ProposalView[F]:

  lazy val SubaruProposalTypeMapping: ObjectMapping =
    ObjectMapping(SubaruProposalTypeType)(
      SqlField("id", ProposalView.Subaru.Id, key = true, hidden = true),
      SqlField("type", ProposalView.Subaru.CallType),
      SqlField("minPercentTime", ProposalView.MinPercent),
      SqlObject("partnerSplits", Join(ProposalView.Subaru.Id, PartnerSplitTable.ProgramId))
    )

  private val SortSplits: Elab[Unit] =
    Elab.transformChild: child =>
      OrderBy(
        OrderSelections(
          List(
            OrderSelection[IntPercent](PartnerSplitType / "percent", ascending = false),
            OrderSelection[Partner](PartnerSplitType / "partner")
          )
        ),
        child
      )

  lazy val SubaruProposalTypeElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] = {
    case (SubaruProposalTypeType, "partnerSplits", Nil) => SortSplits
  }