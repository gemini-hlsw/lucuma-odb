// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping

import table.TargetView

trait RegionMapping[F[_]] extends TargetView[F] {

  private lazy val OpportunityRegionMapping =
    ObjectMapping(OpportunityType / "region")(
      SqlField("synthetic_id", TargetView.Opportunity.Region.SyntheticId, key = true, hidden = true),
      SqlObject("rightAscensionArc"),
      SqlObject("declinationArc"),    
    )

  lazy val RegionMappings = List(
    OpportunityRegionMapping
  )

}

