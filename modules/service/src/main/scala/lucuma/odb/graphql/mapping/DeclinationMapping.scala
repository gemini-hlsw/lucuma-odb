// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.skunk.SkunkMapping
import io.circe
import lucuma.core.math.Declination
import lucuma.odb.graphql.table.ObservationView
import skunk.codec.all.*

import scala.reflect.ClassTag

import table.TargetView

trait DeclinationMapping[F[_]] extends ObservationView[F] with TargetView[F] {

  private def declinationMapping(
    idColumn:    ColumnRef,
    valueColumn: ColumnRef
  ): ObjectMapping =
    ObjectMapping(
      tpe = DeclinationType,
      fieldMappings = List(
        SqlField("synthetic_id", idColumn, key = true, hidden = true),
        SqlField("value", valueColumn, hidden = true),
        FieldRef[Declination]("value").as("dms", Declination.fromStringSignedDMS.reverseGet),
        FieldRef[Declination]("value").as("degrees", c => BigDecimal(c.toAngle.toDoubleDegrees)),
        FieldRef[Declination]("value").as("microarcseconds", _.toAngle.toMicroarcseconds),
      )
    )

  lazy val DeclinationMapping: TypeMapping =
    SwitchMapping(
      DeclinationType,
      List(
        CoordinatesType / "dec" -> declinationMapping(ObservationView.TargetEnvironment.Coordinates.SyntheticId, ObservationView.TargetEnvironment.Coordinates.Dec),
        SiderealType / "dec"    -> declinationMapping(TargetView.Sidereal.SyntheticId, TargetView.Sidereal.Dec),
      )
    )

}

