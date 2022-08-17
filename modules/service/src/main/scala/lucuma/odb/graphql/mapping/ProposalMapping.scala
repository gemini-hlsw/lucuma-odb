// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.odb.graphql.table.PartnerSplitTable
import lucuma.odb.graphql.table.ProposalTable
import lucuma.odb.graphql.util.MappingExtras

import table.TargetView
import table.ProgramTable

trait ProposalMapping[F[_]]
  extends PartnerSplitTable[F]
     with ProgramTable[F]
     with ProposalTable[F]
     with MappingExtras[F] { this: SkunkMapping[F] =>

  lazy val ProposalType = schema.ref("Proposal")

  lazy val ProposalMapping =
    ObjectMapping(
      tpe = ProposalType,
      fieldMappings = List(
        SqlField("program_id", ProposalTable.ProgramId, key = true, hidden = true),
        SqlField("title", ProposalTable.Title),
        SqlField("category", ProposalTable.Category),
        SqlField("toOActivation", ProposalTable.TooActivation),
        SqlField("abstract", ProposalTable.Abstrakt),
        // SqlInterfaceMapping("proposalClass", ...)
        SqlObject("partnerSplits", Join(ProposalTable.ProgramId, PartnerSplitTable.ProgramId))
      )
    )

  }

