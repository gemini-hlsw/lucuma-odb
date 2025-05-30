-- Add imaging science requirements tables
ALTER TABLE t_observation
  ADD COLUMN c_img_minimum_fov      d_angle_µas null default null,
  ADD COLUMN c_img_narrow_filters   boolean     null default null,
  ADD COLUMN c_img_broad_filters    boolean     null default null,
  ADD COLUMN c_img_combined_filters boolean     null default null,
-- Make c_science_mode nullable to support the case where no science requirements are set
  ALTER COLUMN c_science_mode DROP NOT NULL,
  ALTER COLUMN c_science_mode DROP DEFAULT;

-- Rename spec constraint
-- the exposure time mode is shared between imaging and spectroscopy
ALTER TABLE t_observation RENAME COLUMN c_spec_exp_time_mode      TO c_exp_time_mode;
ALTER TABLE t_observation RENAME COLUMN c_spec_signal_to_noise    TO c_etm_signal_to_noise;
ALTER TABLE t_observation RENAME COLUMN c_spec_signal_to_noise_at TO c_etm_signal_to_noise_at;
ALTER TABLE t_observation RENAME COLUMN c_spec_exp_time           TO c_etm_exp_time;
ALTER TABLE t_observation RENAME COLUMN c_spec_exp_count          TO c_etm_exp_count;

-- Add check constraint to ensure c_science_mode is consistent with spectroscopy and imaging fields
ALTER TABLE t_observation
  ADD CONSTRAINT science_mode_consistency CHECK (
    CASE c_science_mode
      -- If spectroscopy mode, all imaging fields must be NULL
      WHEN 'spectroscopy' THEN
        c_img_minimum_fov IS NULL AND
        c_img_narrow_filters IS NULL AND
        c_img_broad_filters IS NULL AND
        c_img_combined_filters IS NULL
      -- If imaging mode, all spectroscopy fields must be NULL
      WHEN 'imaging' THEN
        c_spec_wavelength IS NULL AND
        c_spec_resolution IS NULL AND
        c_spec_wavelength_coverage IS NULL AND
        c_spec_focal_plane IS NULL AND
        c_spec_focal_plane_angle IS NULL AND
        c_spec_capability IS NULL
      -- If science mode is NULL, all science requirement fields must be NULL
      ELSE
        c_spec_wavelength IS NULL AND
        c_spec_resolution IS NULL AND
        c_spec_wavelength_coverage IS NULL AND
        c_spec_focal_plane IS NULL AND
        c_spec_focal_plane_angle IS NULL AND
        c_spec_capability IS NULL AND
        c_img_minimum_fov IS NULL AND
        c_img_narrow_filters IS NULL AND
        c_img_broad_filters IS NULL AND
        c_img_combined_filters IS NULL
    END
  );

-- Create a trigger to automatically set science mode based on field values
CREATE OR REPLACE FUNCTION auto_set_science_mode()
RETURNS TRIGGER AS $$
BEGIN
  -- Check if any spectroscopy fields are set
  IF NEW.c_spec_wavelength IS NOT NULL OR
     NEW.c_spec_resolution IS NOT NULL OR
     NEW.c_spec_wavelength_coverage IS NOT NULL OR
     NEW.c_spec_focal_plane IS NOT NULL OR
     NEW.c_spec_focal_plane_angle IS NOT NULL OR
     NEW.c_spec_capability IS NOT NULL THEN
    -- If spectroscopy fields exist, set mode to spectroscopy
    NEW.c_science_mode := 'spectroscopy';
    -- Check if any imaging fields are set
  ELSIF NEW.c_img_minimum_fov IS NOT NULL OR
        NEW.c_img_narrow_filters IS NOT NULL OR
        NEW.c_img_broad_filters IS NOT NULL OR
        NEW.c_img_combined_filters IS NOT NULL THEN
    -- If imaging fields exist, set mode to imaging
    NEW.c_science_mode := 'imaging';
  ELSE
    -- If no fields are set, set mode to null
    NEW.c_science_mode := NULL;
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Create trigger for inserts and updates
CREATE TRIGGER observation_auto_science_mode
  BEFORE INSERT OR UPDATE ON t_observation
  FOR EACH ROW
  EXECUTE FUNCTION auto_set_science_mode();

-- Update views to include imaging requirements
DROP VIEW v_observation;
CREATE OR REPLACE VIEW v_observation AS
  SELECT o.*,
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
  CASE WHEN o.c_science_mode = 'imaging'      THEN o.c_observation_id END AS c_imaging_mode_id,
  CASE WHEN o.c_science_mode = 'spectroscopy' THEN o.c_observation_id END AS c_spectroscopy_mode_id,
  c.c_active_start::timestamp + (c.c_active_end::timestamp - c.c_active_start::timestamp) * 0.5 AS c_reference_time
  FROM t_observation o
  LEFT JOIN t_proposal p on p.c_program_id = o.c_program_id
  LEFT JOIN t_cfp c on p.c_cfp_id = c.c_cfp_id
  LEFT JOIN t_gmos_north_long_slit mode_gnls ON o.c_observation_id = mode_gnls.c_observation_id
  LEFT JOIN t_gmos_south_long_slit mode_gsls ON o.c_observation_id = mode_gsls.c_observation_id;

-- Add acquisition reset to generator params view
DROP VIEW v_generator_params;
CREATE VIEW v_generator_params AS
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

