// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.syntax.all.*
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

  private def rightAscensionMapping(
    idColumn:    ColumnRef,
    valueColumn: ColumnRef
  ): ObjectMapping =
    ObjectMapping(
      tpe = RightAscensionType,
      fieldMappings = List(
        SqlField("synthetic_id", idColumn, key = true, hidden = true),
        SqlField("value", valueColumn, hidden = true),
        FieldRef[RightAscension]("value").as("hms", RightAscension.fromStringHMS.reverseGet),
        FieldRef[RightAscension]("value").as("hours", c => BigDecimal(c.toHourAngle.toDoubleHours)),
        FieldRef[RightAscension]("value").as("degrees", c => BigDecimal(c.toAngle.toDoubleDegrees)),
        FieldRef[RightAscension]("value").as("microarcseconds", _.toAngle.toMicroarcseconds),
        FieldRef[RightAscension]("value").as("microseconds", _.toHourAngle.toMicroseconds)
      )
    )

  lazy val RightAscensionMapping: TypeMapping =
    SwitchMapping(
      RightAscensionType,
      List(
        CallForProposalsType / "raLimitStart" -> rightAscensionMapping(CallForProposalsView.RaStartId, CallForProposalsView.RaStart),
        CallForProposalsType / "raLimitEnd"   -> rightAscensionMapping(CallForProposalsView.RaEndId, CallForProposalsView.RaEnd),
        CoordinatesType / "ra" -> rightAscensionMapping(ObservationView.TargetEnvironment.Coordinates.SyntheticId, ObservationView.TargetEnvironment.Coordinates.Ra),
        SiderealType / "ra"    -> rightAscensionMapping(TargetView.Sidereal.SyntheticId, TargetView.Sidereal.Ra),
      )
    )

}

