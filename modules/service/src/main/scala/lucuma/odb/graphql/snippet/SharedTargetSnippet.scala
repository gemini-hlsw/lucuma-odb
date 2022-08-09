// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package snippet

import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.odb.graphql.util.MappingExtras
import lucuma.odb.graphql.util.SnippetMapping
import lucuma.odb.util.Codecs.declination
import lucuma.odb.util.Codecs.observation_id
import lucuma.odb.util.Codecs.right_ascension
import lucuma.odb.util.Codecs.target_id

object SharedTargetSnippet {

  def apply[F[_]](
    m: SnippetMapping[F] with MappingExtras[F] with SkunkMapping[F]
  ): m.Snippet = {

    import m.ColumnRef
    import m.FieldRef
    import m.ObjectMapping
    import m.PrefixedMapping
    import m.Snippet
    import m.SqlField
    import m.TableDef
    import m.TypeMapping
    import m.col
    import m.schema

    val RightAscensionType = schema.ref("RightAscension")
    val DeclinationType    = schema.ref("Declination")

    object ObservationView extends TableDef("v_observation") {
      object TargetEnvironment {
        object Coordinates {
          val SyntheticId: ColumnRef = col("c_explicit_base_id",  observation_id.embedded)
          val Ra: ColumnRef          = col("c_explicit_ra",       right_ascension.embedded)
          val Dec: ColumnRef         = col("c_explicit_dec",      declination.embedded)
        }
      }
    }

    object TargetView extends TableDef("v_target") {
      object Sidereal {
        val SyntheticId: ColumnRef = col("c_sidereal_id", target_id.embedded)
        val Ra: ColumnRef          = col("c_sid_ra",      right_ascension.embedded)
        val Dec: ColumnRef         = col("c_sid_dec",     declination.embedded)
      }
    }

    def rightAscensionMapping(
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
        )
      )

    def declinationMapping(
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

    val typeMappings: List[TypeMapping] =
      List(
        PrefixedMapping(
          tpe = RightAscensionType,
          mappings = List(
            List("explicitBase", "ra") -> rightAscensionMapping(ObservationView.TargetEnvironment.Coordinates.SyntheticId, ObservationView.TargetEnvironment.Coordinates.Ra),
            List("sidereal",     "ra") -> rightAscensionMapping(TargetView.Sidereal.SyntheticId, TargetView.Sidereal.Ra)
          )
        ),
        PrefixedMapping(
          tpe = DeclinationType,
          mappings = List(
            List("explicitBase", "dec") -> declinationMapping(ObservationView.TargetEnvironment.Coordinates.SyntheticId, ObservationView.TargetEnvironment.Coordinates.Dec),
            List("sidereal",     "dec") -> declinationMapping(TargetView.Sidereal.SyntheticId, TargetView.Sidereal.Dec)
          )
        )
      )

    Snippet(typeMappings)
  }

}
