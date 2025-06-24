// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping

import table.TargetView

trait OpportunityMapping[F[_]] extends TargetView[F] {

  lazy val OpportunityMapping =
    ObjectMapping(OpportunityType)(
      SqlField("synthetic_id", TargetView.Opportunity.SyntheticId, key = true, hidden = true),
      SqlObject("region"),
    )

}

