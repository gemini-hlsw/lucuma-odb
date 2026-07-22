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
import lucuma.odb.graphql.table.GhostIfuView
import lucuma.odb.graphql.table.GoaDuplicationView
import lucuma.odb.graphql.table.GoaMatchView
import lucuma.odb.graphql.table.ObservationView
import lucuma.odb.graphql.table.TargetView

import scala.reflect.ClassTag

trait DeclinationMapping[F[_]] extends CallForProposalsView[F]
                                  with ConfigurationRequestView[F]
                                  with GhostIfuView[F]
                                  with GoaDuplicationView[F]
                                  with GoaMatchView[F]
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
      declinationMappingAtPath(GeminiCallPropertiesType / "coordinateLimits" / "north" / "decStart", CallForProposalsView.gemini.Id, CallForProposalsView.coordinateLimits.north.DecStart),
      declinationMappingAtPath(GeminiCallPropertiesType / "coordinateLimits" / "north" / "decEnd",   CallForProposalsView.gemini.Id, CallForProposalsView.coordinateLimits.north.DecEnd),
      declinationMappingAtPath(GeminiCallPropertiesType / "coordinateLimits" / "south" / "decStart", CallForProposalsView.gemini.Id, CallForProposalsView.coordinateLimits.south.DecStart),
      declinationMappingAtPath(GeminiCallPropertiesType / "coordinateLimits" / "south" / "decEnd",   CallForProposalsView.gemini.Id, CallForProposalsView.coordinateLimits.south.DecEnd),
      declinationMappingAtPath(KeckCallPropertiesType / "coordinateLimits" / "decStart", CallForProposalsView.keck.Id, CallForProposalsView.coordinateLimits.north.DecStart),
      declinationMappingAtPath(KeckCallPropertiesType / "coordinateLimits" / "decEnd",   CallForProposalsView.keck.Id, CallForProposalsView.coordinateLimits.north.DecEnd),
      declinationMappingAtPath(SubaruCallPropertiesType / "coordinateLimits" / "decStart", CallForProposalsView.subaru.Id, CallForProposalsView.coordinateLimits.north.DecStart),
      declinationMappingAtPath(SubaruCallPropertiesType / "coordinateLimits" / "decEnd",   CallForProposalsView.subaru.Id, CallForProposalsView.coordinateLimits.north.DecEnd),
      declinationMappingAtPath(ConfigurationTargetType / "coordinates" / "dec", ConfigurationRequestView.Target.ReferenceCoordinates.SyntheticId, ConfigurationRequestView.Target.ReferenceCoordinates.Dec),
      declinationMappingAtPath(ConfigurationTargetType / "region" / "declinationArc" / "start", ConfigurationRequestView.Target.Region.DeclinationArc.PartialSyntheticId, ConfigurationRequestView.Target.Region.DeclinationArc.Start),
      declinationMappingAtPath(ConfigurationTargetType / "region" / "declinationArc" / "end", ConfigurationRequestView.Target.Region.DeclinationArc.PartialSyntheticId, ConfigurationRequestView.Target.Region.DeclinationArc.End),
      declinationMappingAtPath(CoordinatesType / "dec", ObservationView.TargetEnvironment.Coordinates.SyntheticId, ObservationView.TargetEnvironment.Coordinates.Dec),
      declinationMappingAtPath(GhostIfuType / "skyPosition" / "dec", GhostIfuView.Sky.Id, GhostIfuView.Sky.Dec),
      declinationMappingAtPath(GoaDuplicationType / "searchCoordinates" / "dec", GoaDuplicationView.SearchCoordinates.SyntheticId, GoaDuplicationView.SearchCoordinates.Dec),
      declinationMappingAtPath(GoaMatchType / "coordinates" / "dec", GoaMatchView.Coordinates.SyntheticId, GoaMatchView.Coordinates.Dec),
      declinationMappingAtPath(OpportunityType / "region" / "declinationArc" / "start", TargetView.Opportunity.Region.DeclinationArc.StartEndSyntheticId, TargetView.Opportunity.Region.DeclinationArc.Start),
      declinationMappingAtPath(OpportunityType / "region" / "declinationArc" / "end", TargetView.Opportunity.Region.DeclinationArc.StartEndSyntheticId, TargetView.Opportunity.Region.DeclinationArc.End),
      declinationMappingAtPath(SiderealType / "dec", TargetView.Sidereal.SyntheticId, TargetView.Sidereal.Dec),
    )

}
