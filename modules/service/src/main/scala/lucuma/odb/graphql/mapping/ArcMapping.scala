// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping

import table.TargetView
import lucuma.odb.graphql.table.ConfigurationRequestView

trait ArcMapping[F[_]] extends TargetView[F] with ConfigurationRequestView[F] {

  private lazy val OpportunityRegionRightAscensionArc =
    ObjectMapping(OpportunityType / "region" / "rightAscensionArc")(
      SqlField("synthetic_id", TargetView.Opportunity.Region.RightAscensionArc.SyntheticId, key = true, hidden = true),
      SqlField("type", TargetView.Opportunity.Region.RightAscensionArc.Type),
      SqlObject("start"),
      SqlObject("end"),
    )

  private lazy val OpportunityRegionDeclinationArc =
    ObjectMapping(OpportunityType / "region" / "declinationArc")(
      SqlField("synthetic_id", TargetView.Opportunity.Region.DeclinationArc.SyntheticId, key = true, hidden = true),
      SqlField("type", TargetView.Opportunity.Region.DeclinationArc.Type),
      SqlObject("start"),
      SqlObject("end"),
    )

  private lazy val ConfigurationRequestRegionDeclinationArc =
    ObjectMapping(ConfigurationTargetType / "region" / "declinationArc")(
      SqlField("synthetic_id", ConfigurationRequestView.Target.Region.SyntheticId, key = true, hidden = true),
      SqlField("type", ConfigurationRequestView.Target.Region.DeclinationArc.Type),
      SqlObject("start"),
      SqlObject("end"),
    )

  private lazy val ConfigurationRequestRegionRightAscensionArc =
    ObjectMapping(ConfigurationTargetType / "region" / "rightAscensionArc")(
      SqlField("synthetic_id", ConfigurationRequestView.Target.Region.SyntheticId, key = true, hidden = true),
      SqlField("type", ConfigurationRequestView.Target.Region.RightAscensionArc.Type),
      SqlObject("start"),
      SqlObject("end"),
    )

  lazy val ArcMappings = List(
    OpportunityRegionRightAscensionArc,
    OpportunityRegionDeclinationArc,
    ConfigurationRequestRegionDeclinationArc,
    ConfigurationRequestRegionRightAscensionArc,
  )

}

