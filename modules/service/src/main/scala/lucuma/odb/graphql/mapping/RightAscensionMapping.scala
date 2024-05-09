// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.syntax.all.*
import grackle.Path
import grackle.skunk.SkunkMapping
import io.circe
import lucuma.core.math.RightAscension
import lucuma.odb.graphql.table.CallForProposalsView
import lucuma.odb.graphql.table.ObservationView
import lucuma.odb.graphql.table.TargetView

import scala.reflect.ClassTag

trait RightAscensionMapping[F[_]] extends CallForProposalsView[F]
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
      rightAscensionMappingAtPath(CallForProposalsType / "raLimitStart", CallForProposalsView.RaStartId, CallForProposalsView.RaStart),
      rightAscensionMappingAtPath(CallForProposalsType / "raLimitEnd", CallForProposalsView.RaEndId, CallForProposalsView.RaEnd),
      rightAscensionMappingAtPath(CoordinatesType / "ra", ObservationView.TargetEnvironment.Coordinates.SyntheticId, ObservationView.TargetEnvironment.Coordinates.Ra),
      rightAscensionMappingAtPath(SiderealType / "ra", TargetView.Sidereal.SyntheticId, TargetView.Sidereal.Ra),
    )

}

