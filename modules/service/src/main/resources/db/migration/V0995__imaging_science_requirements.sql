-- Delete the view as we'll drop c_science_mode
DROP VIEW v_observation;
DROP VIEW v_generator_params;

-- Add imaging science requirements tables and drop c_science_mode
ALTER TABLE t_observation
  ADD COLUMN c_img_minimum_fov      d_angle_µas null default null,
  ADD COLUMN c_img_narrow_filters   boolean     null default null,
  ADD COLUMN c_img_broad_filters    boolean     null default null,
  ADD COLUMN c_img_combined_filters boolean     null default null,
  DROP COLUMN c_science_mode;

-- Add constraint to ensure mutual exclusivity between imaging and spectroscopy fields
ALTER TABLE t_observation ADD CONSTRAINT check_science_mode_exclusivity
CHECK (
  -- Either all spectroscopy fields are NULL (num_nulls = 6)
  num_nulls(c_spec_wavelength, c_spec_resolution, c_spec_wavelength_coverage,
            c_spec_focal_plane, c_spec_focal_plane_angle, c_spec_capability) = 6
  OR
  -- Or all imaging fields are NULL (num_nulls = 4)
  num_nulls(c_img_minimum_fov, c_img_narrow_filters,
            c_img_broad_filters, c_img_combined_filters) = 4
  -- This allows all NULL
);

-- Create function to check if spectroscopy mode is active
CREATE OR REPLACE FUNCTION has_spectroscopy_mode(
  spec_wavelength          d_wavelength_pm,
  spec_resolution          integer,
  spec_wavelength_coverage d_wavelength_pm,
  spec_focal_plane         d_tag,
  spec_focal_plane_angle   d_angle_µas,
  spec_capability          d_tag
) RETURNS boolean AS $$
BEGIN
  RETURN num_nulls(spec_wavelength, spec_resolution, spec_wavelength_coverage,
                   spec_focal_plane, spec_focal_plane_angle, spec_capability) < 6;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

-- Create function to check if imaging mode is active
CREATE OR REPLACE FUNCTION has_imaging_mode(
  img_minimum_fov          d_angle_µas,
  img_narrow_filters       boolean,
  img_broad_filters        boolean,
  img_combined_filters     boolean
) RETURNS boolean AS $$
BEGIN
  RETURN num_nulls(img_minimum_fov, img_narrow_filters,
                   img_broad_filters, img_combined_filters) < 4;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

-- Create function to calculate science mode
CREATE OR REPLACE FUNCTION calculate_science_mode(
  spec_wavelength          d_wavelength_pm,
  spec_resolution          integer,
  spec_wavelength_coverage d_wavelength_pm,
  spec_focal_plane         d_tag,
  spec_focal_plane_angle   d_angle_µas,
  spec_capability          d_tag,
  img_minimum_fov          d_angle_µas,
  img_narrow_filters       boolean,
  img_broad_filters        boolean,
  img_combined_filters     boolean
) RETURNS d_tag AS $$
BEGIN
  -- Check if any spectroscopy fields are populated
  IF has_spectroscopy_mode(spec_wavelength, spec_resolution, spec_wavelength_coverage,
                           spec_focal_plane, spec_focal_plane_angle, spec_capability) THEN
    RETURN 'spectroscopy'::d_tag;
  -- Check if any imaging fields are populated
  ELSIF has_imaging_mode(img_minimum_fov, img_narrow_filters,
                         img_broad_filters, img_combined_filters) THEN
    RETURN 'imaging'::d_tag;
  ELSE
    RETURN NULL;
  END IF;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

-- Rename spec constraint
-- the exposure time mode is shared between imaging and spectroscopy
ALTER TABLE t_observation RENAME COLUMN c_spec_exp_time_mode      TO c_exp_time_mode;
ALTER TABLE t_observation RENAME COLUMN c_spec_signal_to_noise    TO c_etm_signal_to_noise;
ALTER TABLE t_observation RENAME COLUMN c_spec_signal_to_noise_at TO c_etm_signal_to_noise_at;
ALTER TABLE t_observation RENAME COLUMN c_spec_exp_time           TO c_etm_exp_time;
ALTER TABLE t_observation RENAME COLUMN c_spec_exp_count          TO c_etm_exp_count;

-- Update views to include imaging requirements
CREATE OR REPLACE VIEW v_observation AS
  SELECT o.*,
  -- Calculate science mode using function
  calculate_science_mode(
    o.c_spec_wavelength, o.c_spec_resolution, o.c_spec_wavelength_coverage,
    o.c_spec_focal_plane, o.c_spec_focal_plane_angle, o.c_spec_capability,
    o.c_img_minimum_fov, o.c_img_narrow_filters,
    o.c_img_broad_filters, o.c_img_combined_filters
  ) AS c_science_mode,
  CASE WHEN o.c_explicit_ra              IS NOT NULL THEN o.c_observation_id END AS c_explicit_base_id,
  CASE WHEN o.c_air_mass_min             IS NOT NULL THEN o.c_observation_id END AS c_air_mass_id,
  CASE WHEN o.c_hour_angle_min           IS NOT NULL THEN o.c_observation_id END AS c_hour_angle_id,
  CASE WHEN o.c_observing_mode_type      IS NOT NULL THEN o.c_observation_id END AS c_observing_mode_id,
  CASE WHEN o.c_spec_wavelength          IS NOT NULL THEN o.c_observation_id END AS c_spec_wavelength_id,

  CASE WHEN o.c_exp_time_mode            IS NOT NULL THEN o.c_observation_id END AS c_exp_time_mode_id,
  CASE WHEN o.c_exp_time_mode = 'signal_to_noise' THEN o.c_observation_id END AS c_etm_signal_to_noise_id,
  CASE WHEN o.c_exp_time_mode = 'time_and_count'  THEN o.c_observation_id END AS c_etm_time_and_count_id,
  CASE WHEN o.c_spec_wavelength_coverage IS NOT NULL THEN o.c_observation_id END AS c_spec_wavelength_coverage_id,
  CASE WHEN o.c_spec_focal_plane_angle   IS NOT NULL THEN o.c_observation_id END AS c_spec_focal_plane_angle_id,
  CASE WHEN o.c_img_minimum_fov          IS NOT NULL THEN o.c_observation_id END AS c_img_minimum_fov_id,
  CASE WHEN o.c_observation_duration     IS NOT NULL THEN o.c_observation_id END AS c_observation_duration_id,
  -- Use functions for synthetic IDs
  CASE WHEN has_imaging_mode(o.c_img_minimum_fov, o.c_img_narrow_filters,
                             o.c_img_broad_filters, o.c_img_combined_filters)
       THEN o.c_observation_id END AS c_imaging_mode_id,
  -- Synthetic ID for spectroscopy object - null when all spectroscopy fields are null
  CASE WHEN has_spectroscopy_mode(o.c_spec_wavelength, o.c_spec_resolution, o.c_spec_wavelength_coverage,
                                  o.c_spec_focal_plane, o.c_spec_focal_plane_angle, o.c_spec_capability)
       THEN o.c_observation_id END AS c_spectroscopy_mode_id,
  c.c_active_start::timestamp + (c.c_active_end::timestamp - c.c_active_start::timestamp) * 0.5 AS c_reference_time
  FROM t_observation o
  LEFT JOIN t_proposal p on p.c_program_id = o.c_program_id
  LEFT JOIN t_cfp c on p.c_cfp_id = c.c_cfp_id
  LEFT JOIN t_gmos_north_long_slit mode_gnls ON o.c_observation_id = mode_gnls.c_observation_id
  LEFT JOIN t_gmos_south_long_slit mode_gsls ON o.c_observation_id = mode_gsls.c_observation_id;

-- Recreate v_generator_params view
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

