// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Path
import grackle.skunk.SkunkMapping
import io.circe
import lucuma.core.math.Declination
import lucuma.odb.graphql.table.CallForProposalsView
import lucuma.odb.graphql.table.ConfigurationRequestView
import lucuma.odb.graphql.table.ObservationView
import lucuma.odb.graphql.table.TargetView

import scala.reflect.ClassTag

trait DeclinationMapping[F[_]] extends CallForProposalsView[F]
                                  with ConfigurationRequestView[F]
                                  with ObservationView[F]
                                  with TargetView[F] {

  private def declinationMappingAtPath(
    path: Path,
    idColumn:    ColumnRef,
    valueColumn: ColumnRef
  ): ObjectMapping =
    ObjectMapping(path)(
      SqlField("synthetic_id", idColumn, key = true, hidden = true),
      SqlField("value", valueColumn, hidden = true),
      FieldRef[Declination]("value").as("dms", Declination.fromStringSignedDMS.reverseGet),
      FieldRef[Declination]("value").as("degrees", c => BigDecimal(c.toAngle.toDoubleDegrees)),
      FieldRef[Declination]("value").as("microarcseconds", _.toAngle.toMicroarcseconds),
    )

  lazy val DeclinationMapping: List[TypeMapping] =
    List(
      declinationMappingAtPath(CallForProposalsType / "coordinateLimits" / "north" / "decStart", CallForProposalsView.Id, CallForProposalsView.coordinateLimits.north.DecStart),
      declinationMappingAtPath(CallForProposalsType / "coordinateLimits" / "north" / "decEnd",   CallForProposalsView.Id, CallForProposalsView.coordinateLimits.north.DecEnd),
      declinationMappingAtPath(CallForProposalsType / "coordinateLimits" / "south" / "decStart", CallForProposalsView.Id, CallForProposalsView.coordinateLimits.south.DecStart),
      declinationMappingAtPath(CallForProposalsType / "coordinateLimits" / "south" / "decEnd",   CallForProposalsView.Id, CallForProposalsView.coordinateLimits.south.DecEnd),
      declinationMappingAtPath(CoordinatesType / "dec", ObservationView.TargetEnvironment.Coordinates.SyntheticId, ObservationView.TargetEnvironment.Coordinates.Dec),
      declinationMappingAtPath(SiderealType / "dec", TargetView.Sidereal.SyntheticId, TargetView.Sidereal.Dec),
      declinationMappingAtPath(ConfigurationTargetType / "coordinates" / "dec", ConfigurationRequestView.Target.ReferenceCoordinates.SyntheticId, ConfigurationRequestView.Target.ReferenceCoordinates.Dec),
      declinationMappingAtPath(ConfigurationTargetType / "region" / "declinationArc" / "start", ConfigurationRequestView.Target.Region.DeclinationArc.PartialSyntheticId, ConfigurationRequestView.Target.Region.DeclinationArc.Start),
      declinationMappingAtPath(ConfigurationTargetType / "region" / "declinationArc" / "end", ConfigurationRequestView.Target.Region.DeclinationArc.PartialSyntheticId, ConfigurationRequestView.Target.Region.DeclinationArc.End),
      declinationMappingAtPath(OpportunityType / "region" / "declinationArc" / "start", TargetView.Opportunity.Region.DeclinationArc.StartEndSyntheticId, TargetView.Opportunity.Region.DeclinationArc.Start),
      declinationMappingAtPath(OpportunityType / "region" / "declinationArc" / "end", TargetView.Opportunity.Region.DeclinationArc.StartEndSyntheticId, TargetView.Opportunity.Region.DeclinationArc.End),
    )

}

