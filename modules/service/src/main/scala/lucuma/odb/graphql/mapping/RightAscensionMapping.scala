// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.effect.kernel.Resource
import cats.effect.kernel.Sync
import cats.syntax.all._
import edu.gemini.grackle.skunk.SkunkMapping
import edu.gemini.grackle.skunk.SkunkMonitor
import edu.gemini.grackle.sql.SqlMapping
import io.circe
import lucuma.core.math.RightAscension
import lucuma.odb.graphql.table.ObservationView
import lucuma.odb.graphql.util.MappingExtras
import lucuma.odb.util.Codecs._
import skunk.Session
import skunk.circe.codec.all._
import skunk.codec.all._

import scala.reflect.ClassTag

import table.TargetView

trait RightAscensionMapping[F[_]] extends ObservationView[F] with TargetView[F] {

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
        FieldRef[RightAscension]("value").as("microarcseconds", _.toAngle.toMicroarcseconds)
      )
    )

  lazy val RightAscensionMapping: TypeMapping =
    SwitchMapping(
      RightAscensionType,
      List(
        (CoordinatesType, "ra", rightAscensionMapping(ObservationView.TargetEnvironment.Coordinates.SyntheticId, ObservationView.TargetEnvironment.Coordinates.Ra)),
        (SiderealType,    "ra", rightAscensionMapping(TargetView.Sidereal.SyntheticId, TargetView.Sidereal.Ra))
      )
    )

}

