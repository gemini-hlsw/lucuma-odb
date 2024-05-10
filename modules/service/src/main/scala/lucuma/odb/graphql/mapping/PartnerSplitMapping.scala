// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping
import lucuma.odb.graphql.table.PartnerSplitTable

trait PartnerSplitMapping[F[_]] extends PartnerSplitTable[F] {

  lazy val PartnerSplitMapping =
    ObjectMapping(PartnerSplitType)(
      SqlField("program_id", PartnerSplitTable.ProgramId, key = true, hidden = true),
      SqlField("partner", PartnerSplitTable.Partner, key = true),
      SqlField("percent", PartnerSplitTable.Percent),
    )

  }

