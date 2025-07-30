// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GmosCodecs.*

trait ConfigurationRequestView[F[_]] extends BaseMapping[F]:

  object ConfigurationRequestView extends TableDef("v_configuration_request"):

    val Id = col("c_configuration_request_id", configuration_request_id)
    val ProgramId = col("c_program_id", program_id)
    val Status = col("c_status", configuration_request_status)
    val Justification = col("c_justification", text_nonempty.opt)

    object Conditions:
      val CloudExtinction = col("c_cloud_extinction", cloud_extinction_preset)
      val ImageQuality = col("c_image_quality", image_quality_preset)
      val SkyBackground = col("c_sky_background", sky_background)
      val WaterVapor = col("c_water_vapor", water_vapor)

    object Target:
      val Id = col("c_configuration_request_id", configuration_request_id.embedded)
      object ReferenceCoordinates:
        val SyntheticId = col("c_reference_id", configuration_request_id.embedded)
        val Ra = col("c_reference_ra", right_ascension.embedded)
        val Dec = col("c_reference_dec", declination.embedded)

      object Region:
        val SyntheticId = col("c_region_id", configuration_request_id.embedded)
        object RightAscensionArc:
          val PartialSyntheticId = col("c_partial_ra_region_id", configuration_request_id.embedded)
          val Type  = col("c_region_ra_arc_type", arc_type.embedded)
          val Start = col("c_region_ra_arc_start", right_ascension.embedded)
          val End   = col("c_region_ra_arc_end", right_ascension.embedded)
        object DeclinationArc:
          val PartialSyntheticId = col("c_partial_dec_region_id", configuration_request_id.embedded)
          val Type  = col("c_region_dec_arc_type", arc_type.embedded)
          val Start = col("c_region_dec_arc_start", declination.embedded)
          val End   = col("c_region_dec_arc_end", declination.embedded)

    val ObservingModeType = col("c_observing_mode_type", observing_mode_type)

    object GmosNorthLongSlit:
      val Id = col("c_gmos_north_longslit_id", configuration_request_id.embedded)
      val Grating = col("c_gmos_north_longslit_grating", gmos_north_grating.embedded)

    object GmosSouthLongSlit:
      val Id = col("c_gmos_south_longslit_id", configuration_request_id.embedded)
      val Grating = col("c_gmos_south_longslit_grating", gmos_south_grating.embedded)

