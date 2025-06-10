-- GMOS North Imaging Mode table
CREATE TABLE t_gmos_north_imaging (
  c_observation_id       d_observation_id    NOT NULL REFERENCES t_observation(c_observation_id) ON DELETE CASCADE,
  c_observing_mode_type  e_observing_mode_type NOT NULL DEFAULT 'gmos_north_imaging'::e_observing_mode_type,
  c_instrument           d_tag NOT NULL DEFAULT 'GmosNorth',

  -- Explicit overrides for defaults (can be NULL to use computed defaults)
  c_explicit_bin           d_tag             NULL REFERENCES t_gmos_binning(c_tag),
  c_explicit_amp_read_mode d_tag             NULL REFERENCES t_gmos_amp_read_mode(c_tag),
  c_explicit_amp_gain      d_tag             NULL REFERENCES t_gmos_amp_gain(c_tag),
  c_explicit_roi           d_tag             NULL REFERENCES t_gmos_roi(c_tag),

  PRIMARY KEY (c_observation_id, c_observing_mode_type),
  FOREIGN KEY (c_observation_id, c_instrument, c_observing_mode_type) REFERENCES t_observation(c_observation_id, c_instrument, c_observing_mode_type) ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED
)

  CHECK (c_observing_mode_type = 'gmos_north_imaging'),
  CHECK (c_instrument = 'GmosNorth')
);

COMMENT ON TABLE t_gmos_north_imaging IS 'GMOS North Imaging mode configuration';

-- GMOS South Imaging Mode table
CREATE TABLE t_gmos_south_imaging (
  c_observation_id       d_observation_id    NOT NULL REFERENCES t_observation(c_observation_id) ON DELETE CASCADE,
  c_observing_mode_type  e_observing_mode_type NOT NULL DEFAULT 'gmos_south_imaging'::e_observing_mode_type,
  c_instrument           d_tag NOT NULL DEFAULT 'GmosSouth',

  -- Explicit overrides for defaults (can be NULL to use computed defaults)
  c_explicit_bin           d_tag             NULL REFERENCES t_gmos_binning(c_tag),
  c_explicit_amp_read_mode d_tag             NULL REFERENCES t_gmos_amp_read_mode(c_tag),
  c_explicit_amp_gain      d_tag             NULL REFERENCES t_gmos_amp_gain(c_tag),
  c_explicit_roi           d_tag             NULL REFERENCES t_gmos_roi(c_tag),

  PRIMARY KEY (c_observation_id, c_observing_mode_type),
  FOREIGN KEY (c_observation_id, c_instrument, c_observing_mode_type) REFERENCES t_observation(c_observation_id, c_instrument, c_observing_mode_type) ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED

  CONSTRAINT c_gmos_south_imaging_observing_mode_type_check
    CHECK (c_observing_mode_type = 'gmos_south_imaging'),
  CHECK (c_instrument = 'GmosSouth')

);

COMMENT ON TABLE t_gmos_south_imaging IS 'GMOS South Imaging mode configuration';

-- Normalized tables for filter arrays following the d_tag array pattern

-- GMOS North Imaging Filters - normalized table for array support
CREATE TABLE t_gmos_north_imaging_filter (
  c_observation_id d_observation_id NOT NULL REFERENCES t_gmos_north_imaging(c_observation_id) ON DELETE CASCADE,
  c_filter         d_tag            NOT NULL REFERENCES t_gmos_north_filter(c_tag),
  PRIMARY KEY (c_observation_id, c_filter)
);

-- GMOS South Imaging Filters - normalized table for array support
CREATE TABLE t_gmos_south_imaging_filter (
  c_observation_id d_observation_id NOT NULL REFERENCES t_gmos_south_imaging(c_observation_id) ON DELETE CASCADE,
  c_filter         d_tag            NOT NULL REFERENCES t_gmos_south_filter(c_tag),
  PRIMARY KEY (c_observation_id, c_filter)
);

-- GMOS North Imaging filters view
CREATE VIEW v_gmos_north_imaging_filter AS
  SELECT
    c_observation_id,
    array_remove(array_agg(c_filter ORDER BY c_filter), NULL) AS c_filters
  FROM
    t_gmos_north_imaging_filter
  GROUP BY
    c_observation_id;

-- GMOS South Imaging filters view
CREATE VIEW v_gmos_south_imaging_filter AS
  SELECT
    c_observation_id,
    array_remove(array_agg(c_filter ORDER BY c_filter), NULL) AS c_filters
  FROM
    t_gmos_south_imaging_filter
  GROUP BY
    c_observation_id;

-- Complete views with computed defaults for GraphQL
CREATE VIEW v_gmos_north_imaging AS
  SELECT
    i.*,
    f.c_filters,
  FROM
    t_gmos_north_imaging i
  LEFT JOIN v_gmos_north_imaging_filter f
    ON i.c_observation_id = f.c_observation_id;

COMMENT ON VIEW v_gmos_north_imaging IS 'GMOS North Imaging complete view with computed defaults';

CREATE VIEW v_gmos_south_imaging AS
  SELECT
    i.*,
    f.c_filters,
  FROM
    t_gmos_south_imaging i
  LEFT JOIN v_gmos_south_imaging_filter f
    ON i.c_observation_id = f.c_observation_id;

-- Update the observation view to include imaging mode IDs
DROP VIEW IF EXISTS v_generator_params;
DROP VIEW IF EXISTS v_observation;

-- Recreate observation view with imaging mode support
CREATE OR REPLACE VIEW v_observation AS
  SELECT o.*,
  -- Calculate science mode using num_nulls (updated from V0995 to include existing logic)
  CASE
    WHEN num_nulls(o.c_spec_wavelength, o.c_spec_resolution, o.c_spec_wavelength_coverage,
                   o.c_spec_focal_plane, o.c_spec_focal_plane_angle, o.c_spec_capability) < 6
         THEN 'spectroscopy'::d_tag
    WHEN num_nulls(o.c_img_minimum_fov, o.c_img_narrow_filters,
                   o.c_img_broad_filters, o.c_img_combined_filters) < 4
         THEN 'imaging'::d_tag
    ELSE NULL
  END AS c_science_mode,
  CASE WHEN o.c_explicit_ra              IS NOT NULL THEN o.c_observation_id END AS c_explicit_base_id,
  CASE WHEN o.c_air_mass_min             IS NOT NULL THEN o.c_observation_id END AS c_air_mass_id,
  CASE WHEN o.c_hour_angle_min           IS NOT NULL THEN o.c_observation_id END AS c_hour_angle_id,
  CASE WHEN o.c_observing_mode_type      IS NOT NULL THEN o.c_observation_id END AS c_observing_mode_id,
  CASE WHEN o.c_spec_wavelength          IS NOT NULL THEN o.c_observation_id END AS c_spec_wavelength_id,

  CASE WHEN o.c_exp_time_mode            IS NOT NULL THEN o.c_observation_id END AS c_exp_time_mode_id,
  CASE WHEN o.c_exp_time_mode = 'signal_to_noise'    THEN o.c_observation_id END AS c_etm_signal_to_noise_id,
  CASE WHEN o.c_exp_time_mode = 'time_and_count'     THEN o.c_observation_id END AS c_etm_time_and_count_id,
  CASE WHEN o.c_spec_wavelength_coverage IS NOT NULL THEN o.c_observation_id END AS c_spec_wavelength_coverage_id,
  CASE WHEN o.c_spec_focal_plane_angle   IS NOT NULL THEN o.c_observation_id END AS c_spec_focal_plane_angle_id,
  CASE WHEN o.c_img_minimum_fov          IS NOT NULL THEN o.c_observation_id END AS c_img_minimum_fov_id,
  CASE WHEN o.c_observation_duration     IS NOT NULL THEN o.c_observation_id END AS c_observation_duration_id,
  CASE WHEN num_nulls(o.c_img_minimum_fov, o.c_img_narrow_filters, o.c_img_broad_filters, o.c_img_combined_filters) < 4 THEN o.c_observation_id END AS c_imaging_mode_id,
  CASE WHEN num_nulls(o.c_spec_wavelength, o.c_spec_resolution, o.c_spec_wavelength_coverage, o.c_spec_focal_plane, o.c_spec_focal_plane_angle, o.c_spec_capability) < 6 THEN o.c_observation_id END AS c_spectroscopy_mode_id,
  CASE WHEN mode_gni.c_observation_id IS NOT NULL THEN o.c_observation_id END AS c_gmos_north_imaging_id,
  CASE WHEN mode_gsi.c_observation_id IS NOT NULL THEN o.c_observation_id END AS c_gmos_south_imaging_id,
  c.c_active_start::timestamp + (c.c_active_end::timestamp - c.c_active_start::timestamp) * 0.5 AS c_reference_time
  FROM t_observation o
  LEFT JOIN t_proposal p on p.c_program_id = o.c_program_id
  LEFT JOIN t_cfp c on p.c_cfp_id = c.c_cfp_id
  LEFT JOIN t_gmos_north_long_slit mode_gnls ON o.c_observation_id = mode_gnls.c_observation_id
  LEFT JOIN t_gmos_south_long_slit mode_gsls ON o.c_observation_id = mode_gsls.c_observation_id
  LEFT JOIN t_gmos_north_imaging mode_gni ON o.c_observation_id = mode_gni.c_observation_id
  LEFT JOIN t_gmos_south_imaging mode_gsi ON o.c_observation_id = mode_gsi.c_observation_id;

-- Recreate v_generator_params view (same as in V0995)
CREATE OR REPLACE VIEW v_generator_params AS
SELECT
  o.c_program_id,
  o.c_observation_id,
  o.c_calibration_role,
  o.c_image_quality,
  o.c_cloud_extinction,
  o.c_sky_background,
  o.c_water_vapor,
  o.c_air_mass_min,
  o.c_air_mass_max,
  o.c_hour_angle_min,
  o.c_hour_angle_max,
  o.c_exp_time_mode,
  o.c_etm_signal_to_noise,
  o.c_etm_signal_to_noise_at,
  o.c_etm_exp_time,
  o.c_etm_exp_count,
  o.c_observing_mode_type,
  o.c_science_band,
  o.c_declared_complete,
  o.c_acq_reset_time,
  t.c_target_id,
  t.c_sid_rv,
  t.c_source_profile
FROM
  t_observation o
LEFT JOIN t_asterism_target a
  ON o.c_observation_id = a.c_observation_id
LEFT JOIN t_target t
  ON  a.c_target_id  = t.c_target_id
  AND t.c_existence <> 'deleted'
ORDER BY
  o.c_observation_id,
  t.c_target_id;
