// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.snippet
package mapping

import edu.gemini.grackle.sql.SqlMapping
import lucuma.core.math.Declination
import io.circe
import scala.reflect.ClassTag
import cats.syntax.all._
import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.odb.util.Codecs._
import skunk.codec.all._
import skunk.circe.codec.all._
import cats.effect.kernel.Sync
import cats.effect.kernel.Resource
import edu.gemini.grackle.skunk.SkunkMonitor
import skunk.Session
import table.TargetView
import lucuma.odb.graphql.util.MappingExtras
import lucuma.odb.graphql.snippet.table.ObservationView

trait DeclinationMapping[F[_]]
  extends ObservationView[F]
     with TargetView[F]
     with MappingExtras[F]
  { this: SkunkMapping[F]  =>

  lazy val DeclinationType = schema.ref("Declination")

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

  lazy val DeclinationMapping =
    PrefixedMapping(
      tpe = DeclinationType,
      mappings = List(
        // Observation
        // List("explicitBase", "dec") -> declinationMapping(ObservationView.TargetEnvironment.Coordinates.SyntheticId, ObservationView.TargetEnvironment.Coordinates.Dec),
        // Target
        List("sidereal", "dec") -> declinationMapping(TargetView.Sidereal.SyntheticId, TargetView.Sidereal.Dec)
      )
    )

}

