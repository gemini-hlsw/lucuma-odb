// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Path
import grackle.skunk.SkunkMapping
import io.circe
import lucuma.core.math.RightAscension
import lucuma.odb.graphql.table.CallForProposalsView
import lucuma.odb.graphql.table.ConfigurationRequestView
import lucuma.odb.graphql.table.ObservationView
import lucuma.odb.graphql.table.TargetView

trait RightAscensionMapping[F[_]] extends CallForProposalsView[F]
                                     with ConfigurationRequestView[F]
                                     with ObservationView[F]
                                     with TargetView[F] {

  private def rightAscensionMappingAtPath(
    path: Path,
    idColumn:    ColumnRef,
    valueColumn: ColumnRef
  ): ObjectMapping =
    ObjectMapping(path)(
      SqlField("synthetic_id", idColumn, key = true, hidden = true),
      SqlField("value", valueColumn, hidden = true),
      FieldRef[RightAscension]("value").as("hms", RightAscension.fromStringHMS.reverseGet),
      FieldRef[RightAscension]("value").as("hours", c => BigDecimal(c.toHourAngle.toDoubleHours)),
      FieldRef[RightAscension]("value").as("degrees", c => BigDecimal(c.toAngle.toDoubleDegrees)),
      FieldRef[RightAscension]("value").as("microarcseconds", _.toAngle.toMicroarcseconds),
      FieldRef[RightAscension]("value").as("microseconds", _.toHourAngle.toMicroseconds)
    )

  lazy val RightAscensionMappings: List[TypeMapping] =
    List(
      rightAscensionMappingAtPath(CallForProposalsType / "coordinateLimits" / "north" / "raStart", CallForProposalsView.Id, CallForProposalsView.coordinateLimits.north.RaStart),
      rightAscensionMappingAtPath(CallForProposalsType / "coordinateLimits" / "north" / "raEnd",   CallForProposalsView.Id, CallForProposalsView.coordinateLimits.north.RaEnd),
      rightAscensionMappingAtPath(CallForProposalsType / "coordinateLimits" / "south" / "raStart", CallForProposalsView.Id, CallForProposalsView.coordinateLimits.south.RaStart),
      rightAscensionMappingAtPath(CallForProposalsType / "coordinateLimits" / "south" / "raEnd",   CallForProposalsView.Id, CallForProposalsView.coordinateLimits.south.RaEnd),
      rightAscensionMappingAtPath(CoordinatesType / "ra", ObservationView.TargetEnvironment.Coordinates.SyntheticId, ObservationView.TargetEnvironment.Coordinates.Ra),
      rightAscensionMappingAtPath(SiderealType / "ra", TargetView.Sidereal.SyntheticId, TargetView.Sidereal.Ra),
      rightAscensionMappingAtPath(ConfigurationTargetType / "coordinates" / "ra", ConfigurationRequestView.Target.ReferenceCoordinates.SyntheticId, ConfigurationRequestView.Target.ReferenceCoordinates.Ra),
      rightAscensionMappingAtPath(ConfigurationTargetType / "region" / "rightAscensionArc" / "start", ConfigurationRequestView.Target.Region.RightAscensionArc.PartialSyntheticId, ConfigurationRequestView.Target.Region.RightAscensionArc.Start),
      rightAscensionMappingAtPath(ConfigurationTargetType / "region" / "rightAscensionArc" / "end", ConfigurationRequestView.Target.Region.RightAscensionArc.PartialSyntheticId, ConfigurationRequestView.Target.Region.RightAscensionArc.End),
      rightAscensionMappingAtPath(OpportunityType / "region" / "rightAscensionArc" / "start", TargetView.Opportunity.Region.RightAscensionArc.StartEndSyntheticId, TargetView.Opportunity.Region.RightAscensionArc.Start),
      rightAscensionMappingAtPath(OpportunityType / "region" / "rightAscensionArc" / "end", TargetView.Opportunity.Region.RightAscensionArc.StartEndSyntheticId, TargetView.Opportunity.Region.RightAscensionArc.End),
    )

}

