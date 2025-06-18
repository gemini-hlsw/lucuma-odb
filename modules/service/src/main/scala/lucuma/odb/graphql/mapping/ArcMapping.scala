// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping

import table.TargetView

trait ArcMapping[F[_]] extends TargetView[F] {

  private lazy val OpportunityRegionRightAscensionArc =
    ObjectMapping(RightAscensionArcType)(
      SqlField("synthetic_id", TargetView.Opportunity.Region.RightAscensionArc.SyntheticId, key = true, hidden = true),
      SqlField("type", TargetView.Opportunity.Region.RightAscensionArc.Type),
      SqlObject("start"),
      SqlObject("end"),
    )

  private lazy val OpportunityRegionDeclinationArc =
    ObjectMapping(DeclinationArcType)(
      SqlField("synthetic_id", TargetView.Opportunity.Region.DeclinationArc.SyntheticId, key = true, hidden = true),
      SqlField("type", TargetView.Opportunity.Region.DeclinationArc.Type),
      SqlObject("start"),
      SqlObject("end"),
    )

  lazy val ArcMappings = List(
    OpportunityRegionRightAscensionArc,
    OpportunityRegionDeclinationArc,
  )

}

