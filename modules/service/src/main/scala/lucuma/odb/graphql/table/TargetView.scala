// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.*
import skunk.circe.codec.all.*
import skunk.codec.all.*

trait TargetView[F[_]] extends BaseMapping[F] {

  object TargetView extends TableDef("v_target") {
    val ProgramId       = col("c_program_id", program_id)
    val TargetId        = col("c_target_id", target_id)
    val Name            = col("c_name", text_nonempty)
    val Existence       = col("c_existence", existence)
    val SourceProfile   = col("c_source_profile", jsonb)
    val CalibrationRole = col("c_calibration_role", calibration_role.opt)
    object Sidereal {
      val SyntheticId    = col("c_sidereal_id", target_id.embedded)
      val Ra             = col("c_sid_ra", right_ascension.embedded)
      val Dec            = col("c_sid_dec", declination.embedded)
      val Epoch          = col("c_sid_epoch", epoch.embedded)
      object RadialVelocity {
        val SyntheticId = col("c_sid_rv_id", target_id.embedded)
        val Value       = col("c_sid_rv", radial_velocity.embedded)
      }
      object Parallax {
        val SyntheticId = col("c_sid_parallax_id", target_id.embedded)
        val Value = col("c_sid_parallax", parallax.embedded)
      }
      object Catalog {
        val SyntheticId = col("c_sid_catalog_info_id", target_id.embedded)
        val Name        = col("c_sid_catalog_name", catalog_name.embedded)
        val Id          = col("c_sid_catalog_id", varchar.embedded)
        val ObjectType  = col("c_sid_catalog_object_type", varchar.opt)
      }
      object ProperMotion {
        val SyntheticId = col("c_sid_pm_id", target_id.embedded)
        val Ra  = col("c_sid_pm_ra", angle_µas.embedded)
        val Dec = col("c_sid_pm_dec", angle_µas.embedded)
      }
    }
    object Nonsidereal {
      val SyntheticId = col("c_nonsidereal_id", target_id.embedded)
      val Des         = col("c_nsid_des", varchar.embedded)
      val KeyType     = col("c_nsid_key_type", ephemeris_key_type.embedded)
      val Key         = col("c_nsid_key", varchar.embedded)
    }
  }

}
