CREATE DOMAIN d_guide_target_name as text
CHECK(
  -- For now, at least, we only support gaia
  -- The same regex is used in GuideStarName.scala in lucuma-ags - changes should be synchronized.
  VALUE ~  '^Gaia DR3 -?\d+$'
);

ALTER TABLE t_observation
  ADD COLUMN c_observation_duration     interval             NULL,
  ADD COLUMN c_guide_target_name        d_guide_target_name  NULL,
  ADD COLUMN c_guide_target_hash        bytea                NULL;

-- Re-create v_observation to include the new columns and synthetic id for observation duration
DROP VIEW v_observation;
CREATE OR REPLACE VIEW v_observation AS
  SELECT o.*,
  CASE WHEN c_explicit_ra              IS NOT NULL THEN c_observation_id END AS c_explicit_base_id,
  CASE WHEN c_air_mass_min             IS NOT NULL THEN c_observation_id END AS c_air_mass_id,
  CASE WHEN c_hour_angle_min           IS NOT NULL THEN c_observation_id END AS c_hour_angle_id,
  CASE WHEN c_observing_mode_type      IS NOT NULL THEN c_observation_id END AS c_observing_mode_id,
  CASE WHEN c_spec_wavelength          IS NOT NULL THEN c_observation_id END AS c_spec_wavelength_id,
  CASE WHEN c_spec_signal_to_noise_at  IS NOT NULL THEN c_observation_id END AS c_spec_signal_to_noise_at_id,
  CASE WHEN c_spec_wavelength_coverage IS NOT NULL THEN c_observation_id END AS c_spec_wavelength_coverage_id,
  CASE WHEN c_spec_focal_plane_angle   IS NOT NULL THEN c_observation_id END AS c_spec_focal_plane_angle_id,
  CASE WHEN c_observation_duration     IS NOT NULL THEN c_observation_id END AS c_observation_duration_id
  FROM t_observation o;
