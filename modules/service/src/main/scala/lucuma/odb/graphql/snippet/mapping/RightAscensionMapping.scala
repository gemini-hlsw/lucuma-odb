// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.snippet
package mapping

import edu.gemini.grackle.sql.SqlMapping
import lucuma.core.math.RightAscension
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

trait RightAscensionMapping[F[_]] extends TargetView[F] { this: SkunkMapping[F]  =>

  lazy val RightAscensionType = schema.ref("RightAscension")

  object FieldRef {
    def apply[A](underlyingField: String) = new Partial[A](underlyingField)
    class Partial[A](underlyingField: String) {
      def as[B: circe.Encoder](field: String, f: A => B)(implicit ev: ClassTag[A]): CursorField[B] =
        CursorField(field, c => c.field(underlyingField, None).flatMap(_.as[A].map(f)), List(underlyingField))
    }
  }

  lazy val Target_sidereal_ra: ObjectMapping =
    ObjectMapping(
      tpe = RightAscensionType,
      fieldMappings = List(
        SqlField("synthetic_id", TargetView.Sidereal.SyntheticId, key = true, hidden = true),
        SqlField("value", TargetView.Sidereal.Ra, hidden = true),
        FieldRef[RightAscension]("value").as("hms", RightAscension.fromStringHMS.reverseGet),
        FieldRef[RightAscension]("value").as("hours", c => BigDecimal(c.toHourAngle.toDoubleHours)),
        FieldRef[RightAscension]("value").as("degrees", c => BigDecimal(c.toAngle.toDoubleDegrees)),
        FieldRef[RightAscension]("value").as("microarcseconds", _.toAngle.toMicroarcseconds),
      )
    )

  lazy val RightAscensionMapping =
    PrefixedMapping(
      tpe = RightAscensionType,
      mappings = List(
        List("sidereal", "ra") -> Target_sidereal_ra,
      )
    )

}

