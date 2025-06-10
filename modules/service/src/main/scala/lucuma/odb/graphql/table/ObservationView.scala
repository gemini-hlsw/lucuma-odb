// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package table

import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GmosCodecs.*
import skunk.circe.codec.all.*
import skunk.codec.all.*

trait ObservationView[F[_]] extends BaseMapping[F] {

    object ObservationView extends TableDef("v_observation") {
      val ProgramId: ColumnRef           = col("c_program_id",           program_id)
      val Id: ColumnRef                  = col("c_observation_id",       observation_id)
      val ObservationIndex: ColumnRef    = col("c_observation_index",    int4_pos)
      val Existence: ColumnRef           = col("c_existence",            existence)
      val Title: ColumnRef               = col("c_title",                text_nonempty)
      val Subtitle: ColumnRef            = col("c_subtitle",             text_nonempty.opt)
      val Instrument: ColumnRef          = col("c_instrument",           instrument.opt)
      val ScienceBand: ColumnRef         = col("c_science_band",         science_band.opt)
      val ObservationTime: ColumnRef     = col("c_observation_time",     core_timestamp.opt)
      val AsterismGroup: ColumnRef       = col("c_asterism_group",       jsonb)
      val GroupId: ColumnRef             = col("c_group_id",             group_id.opt)
      val GroupIndex: ColumnRef          = col("c_group_index",          int2_nonneg)
      val CalibrationRole: ColumnRef     = col("c_calibration_role",     calibration_role.opt)
      val ObserverNotes: ColumnRef       = col("c_observer_notes",       text_nonempty.opt)
      val ReferenceTime: ColumnRef       = col("c_reference_time",       core_timestamp.opt)
      val DeclaredComplete: ColumnRef    = col("c_declared_complete",    bool)

      object PlannedTime {
        val Pi        = col("c_pts_pi", time_span)
        val Uncharged = col("c_pts_uncharged", time_span)
        val Execution = col("c_pts_execution", time_span)
      }

      object PosAngleConstraint {
        val Mode: ColumnRef  = col("c_pac_mode",  pac_mode.embedded)
        val Angle: ColumnRef = col("c_pac_angle", angle_µas.embedded)
      }

      object TargetEnvironment {
        object Coordinates {
          val SyntheticId: ColumnRef = col("c_explicit_base_id",  observation_id.embedded)
          val Ra: ColumnRef          = col("c_explicit_ra",       right_ascension.embedded)
          val Dec: ColumnRef         = col("c_explicit_dec",      declination.embedded)
        }
      }

      object ConstraintSet {
        val Key: ColumnRef = col("c_conditions_key", text)
        val CloudExtinction: ColumnRef = col("c_cloud_extinction", cloud_extinction_preset.embedded)
        val ImageQuality: ColumnRef    = col("c_image_quality",    image_quality_preset.embedded)
        val SkyBackground: ColumnRef   = col("c_sky_background",   sky_background.embedded)
        val WaterVapor: ColumnRef      = col("c_water_vapor",      water_vapor.embedded)
        object ElevationRange {
          val SyntheticId: ColumnRef   = col("c_hour_angle_id",  observation_id.embedded)
          object AirMassRange {
            val SyntheticId: ColumnRef = col("c_air_mass_id",  observation_id.embedded)
            val AirMassMin: ColumnRef  = col("c_air_mass_min", air_mass_range_value.embedded.withDomain("d_air_mass"))
            val AirMassMax: ColumnRef  = col("c_air_mass_max", air_mass_range_value.embedded.withDomain("d_air_mass"))
          }
          object HourAngleRange {
            val SyntheticId: ColumnRef  = col("c_hour_angle_id",  observation_id.embedded)
            val HourAngleMin: ColumnRef = col("c_hour_angle_min", hour_angle_range_value.embedded.withDomain("d_hour_angle"))
            val HourAngleMax: ColumnRef = col("c_hour_angle_max", hour_angle_range_value.embedded.withDomain("d_hour_angle"))
          }
        }
      }

      object ScienceRequirements:
        val Mode: ColumnRef = col("c_science_mode", science_mode.embedded.opt)

        object Spectroscopy:
          val SyntheticId: ColumnRef = col("c_spectroscopy_mode_id", observation_id.embedded)

          object Wavelength:
            val SyntheticId: ColumnRef = col("c_spec_wavelength_id", observation_id.embedded)
            val Value: ColumnRef       = col("c_spec_wavelength",    wavelength_pm.embedded)

          object ExposureTimeMode:
            val SyntheticId: ColumnRef   = col("c_exp_time_mode_id", observation_id.embedded)

            object SignalToNoise:
              val SyntheticId: ColumnRef = col("c_etm_signal_to_noise_id", observation_id.embedded)
              val Value: ColumnRef       = col("c_etm_signal_to_noise",    signal_to_noise.embedded)
              val At: ColumnRef          = col("c_etm_signal_to_noise_at", wavelength_pm.embedded)

            object TimeAndCount:
              val SyntheticId: ColumnRef = col("c_etm_time_and_count_id",  observation_id.embedded)
              val Time: ColumnRef        = col("c_etm_exp_time",           time_span.embedded)
              val Count: ColumnRef       = col("c_etm_exp_count",          int4_nonneg.embedded)
              val At: ColumnRef          = col("c_etm_signal_to_noise_at", wavelength_pm.embedded)

          object WavelengthCoverage:
            val SyntheticId: ColumnRef = col("c_spec_wavelength_coverage_id", observation_id.embedded)
            val Value: ColumnRef       = col("c_spec_wavelength_coverage",    wavelength_pm.embedded)

          object FocalPlaneAngle:
            val SyntheticId: ColumnRef = col("c_spec_focal_plane_angle_id", observation_id.embedded)
            val Value: ColumnRef       = col("c_spec_focal_plane_angle",    angle_µas.embedded)

          val Resolution: ColumnRef    = col("c_spec_resolution",          int4_pos.opt)
          val FocalPlane: ColumnRef    = col("c_spec_focal_plane",         focal_plane.opt)
          val Capability: ColumnRef    = col("c_spec_capability",          spectroscopy_capabilities.opt)

        object Imaging:
          val SyntheticId: ColumnRef = col("c_imaging_mode_id", observation_id.embedded)

          object MinimumFovAngle:
            val SyntheticId: ColumnRef = col("c_img_minimum_fov_id", observation_id.embedded)
            val Value: ColumnRef       = col("c_img_minimum_fov",    angle_µas.embedded)

          val NarrowFilters: ColumnRef   = col("c_img_narrow_filters", bool.opt)
          val BroadFilters: ColumnRef    = col("c_img_broad_filters",  bool.opt)
          val CombinedFilters: ColumnRef = col("c_img_combined_filters", bool.opt)

          object ExposureTimeMode:
            val SyntheticId: ColumnRef   = col("c_exp_time_mode_id", observation_id.embedded)

            object SignalToNoise:
              val SyntheticId: ColumnRef = col("c_etm_signal_to_noise_id", observation_id.embedded)
              val Value: ColumnRef       = col("c_etm_signal_to_noise",    signal_to_noise.embedded)
              val At: ColumnRef          = col("c_etm_signal_to_noise_at", wavelength_pm.embedded)

            object TimeAndCount:
              val SyntheticId: ColumnRef = col("c_etm_time_and_count_id",  observation_id.embedded)
              val Time: ColumnRef        = col("c_etm_exp_time",           time_span.embedded)
              val Count: ColumnRef       = col("c_etm_exp_count",          int4_nonneg.embedded)
              val At: ColumnRef          = col("c_etm_signal_to_noise_at", wavelength_pm.embedded)

          object ImagingGmosNorthView extends TableDef("v_imaging_requirements_gmos_north"):
            val Id: ColumnRef = col("c_observation_id", observation_id)
            val Filters       = col("c_filters",        _gmos_north_filter)

          object ImagingGmosSouthView extends TableDef("v_imaging_requirements_gmos_south"):
            val Id: ColumnRef = col("c_observation_id", observation_id)
            val Filters       = col("c_filters",        _gmos_south_filter)

      end ScienceRequirements

      object ObservingMode {
        val Key: ColumnRef = col("c_mode_key", text)
        val SyntheticId: ColumnRef = col("c_observing_mode_id", observation_id.embedded)
        val ObservingModeType: ColumnRef = col("c_observing_mode_type", observing_mode_type.embedded)
        
        // GMOS Imaging Mode Synthetic IDs
        val GmosNorthImagingId: ColumnRef = col("c_gmos_north_imaging_id", observation_id.embedded)
        val GmosSouthImagingId: ColumnRef = col("c_gmos_south_imaging_id", observation_id.embedded)
      }

      object ObservationDuration {
        val SyntheticId: ColumnRef = col("c_observation_duration_id", observation_id.embedded)
        val ObservationDuration: ColumnRef = col("c_observation_duration", time_span.embedded)
      }

      // GMOS Imaging Mode Tables and Views
      
      object GmosNorthImagingView extends TableDef("v_gmos_north_imaging"):
        val Id: ColumnRef                   = col("c_observation_id",         observation_id)
        val ObservingModeType: ColumnRef    = col("c_observing_mode_type",    observing_mode_type)
        val ExplicitXBin: ColumnRef         = col("c_explicit_x_bin",         gmos_xbin.opt)
        val ExplicitYBin: ColumnRef         = col("c_explicit_y_bin",         gmos_ybin.opt) 
        val ExplicitAmpReadMode: ColumnRef  = col("c_explicit_amp_read_mode", gmos_amp_read_mode.opt)
        val ExplicitAmpGain: ColumnRef      = col("c_explicit_amp_gain",      gmos_amp_gain.opt)
        val ExplicitRoi: ColumnRef          = col("c_explicit_roi",           gmos_roi.opt)
        val Filters: ColumnRef              = col("c_filters",                _gmos_north_filter)
        val XBin: ColumnRef                 = col("c_x_bin",                  gmos_xbin)
        val YBin: ColumnRef                 = col("c_y_bin",                  gmos_ybin)
        val AmpReadMode: ColumnRef          = col("c_amp_read_mode",          gmos_amp_read_mode)
        val AmpGain: ColumnRef              = col("c_amp_gain",               gmos_amp_gain)
        val Roi: ColumnRef                  = col("c_roi",                    gmos_roi)
        val DefaultXBin: ColumnRef          = col("c_default_x_bin",          gmos_xbin)
        val DefaultYBin: ColumnRef          = col("c_default_y_bin",          gmos_ybin)
        val DefaultAmpReadMode: ColumnRef   = col("c_default_amp_read_mode",  gmos_amp_read_mode)
        val DefaultAmpGain: ColumnRef       = col("c_default_amp_gain",       gmos_amp_gain)
        val DefaultRoi: ColumnRef           = col("c_default_roi",            gmos_roi)

      object GmosSouthImagingView extends TableDef("v_gmos_south_imaging"):
        val Id: ColumnRef                   = col("c_observation_id",         observation_id)
        val ObservingModeType: ColumnRef    = col("c_observing_mode_type",    observing_mode_type)
        val ExplicitXBin: ColumnRef         = col("c_explicit_x_bin",         gmos_xbin.opt)
        val ExplicitYBin: ColumnRef         = col("c_explicit_y_bin",         gmos_ybin.opt)
        val ExplicitAmpReadMode: ColumnRef  = col("c_explicit_amp_read_mode", gmos_amp_read_mode.opt)
        val ExplicitAmpGain: ColumnRef      = col("c_explicit_amp_gain",      gmos_amp_gain.opt)
        val ExplicitRoi: ColumnRef          = col("c_explicit_roi",           gmos_roi.opt)
        val Filters: ColumnRef              = col("c_filters",                _gmos_south_filter)
        val XBin: ColumnRef                 = col("c_x_bin",                  gmos_xbin)
        val YBin: ColumnRef                 = col("c_y_bin",                  gmos_ybin)
        val AmpReadMode: ColumnRef          = col("c_amp_read_mode",          gmos_amp_read_mode)
        val AmpGain: ColumnRef              = col("c_amp_gain",               gmos_amp_gain)
        val Roi: ColumnRef                  = col("c_roi",                    gmos_roi)
        val DefaultXBin: ColumnRef          = col("c_default_x_bin",          gmos_xbin)
        val DefaultYBin: ColumnRef          = col("c_default_y_bin",          gmos_ybin)
        val DefaultAmpReadMode: ColumnRef   = col("c_default_amp_read_mode",  gmos_amp_read_mode)
        val DefaultAmpGain: ColumnRef       = col("c_default_amp_gain",       gmos_amp_gain)
        val DefaultRoi: ColumnRef           = col("c_default_roi",            gmos_roi)

    }

}
