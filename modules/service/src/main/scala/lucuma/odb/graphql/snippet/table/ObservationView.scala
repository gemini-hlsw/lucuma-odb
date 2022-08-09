// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package snippet
package table

import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.odb.util.Codecs._
import skunk.codec.all._

trait ObservationView[F[_]] { self: SkunkMapping[F] =>

  object ObservationView extends TableDef("v_observation") {
    val ProgramId: ColumnRef    = col("c_program_id",          program_id)
    val Id: ColumnRef           = col("c_observation_id",      observation_id)
    val Existence: ColumnRef    = col("c_existence",           existence)
    val Subtitle: ColumnRef     = col("c_subtitle",            text_nonempty.opt)
//      val Instrument: m.ColumnRef   = col("c_instrument", tag.opt)
    val Status: ColumnRef       = col("c_status",              obs_status)
    val ActiveStatus: ColumnRef = col("c_active_status",       obs_active_status)
    object ConstraintSet {
      val CloudExtinction: ColumnRef = col("c_cloud_extinction", cloud_extinction.embedded)
      val ImageQuality: ColumnRef    = col("c_image_quality",    image_quality.embedded)
      val SkyBackground: ColumnRef   = col("c_sky_background",   sky_background.embedded)
      val WaterVapor: ColumnRef      = col("c_water_vapor",      water_vapor.embedded)
      object ElevationRange {
        object AirMassRange {
          val SyntheticId: ColumnRef = col("c_air_mass_id",  observation_id.embedded)
          val AirMassMin: ColumnRef  = col("c_air_mass_min", air_mass_range_value.embedded)
          val AirMassMax: ColumnRef  = col("c_air_mass_max", air_mass_range_value.embedded)
        }
        object HourAngleRange {
          val SyntheticId: ColumnRef  = col("c_hour_angle_id",  observation_id.embedded)
          val HourAngleMin: ColumnRef = col("c_hour_angle_min", hour_angle_range_value.embedded)
          val HourAngleMax: ColumnRef = col("c_hour_angle_max", hour_angle_range_value.embedded)
        }
      }
    }
  }

}