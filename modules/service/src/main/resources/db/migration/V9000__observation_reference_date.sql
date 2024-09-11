
-- Re-create v_observation to include reference time
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
  CASE WHEN c_observation_duration     IS NOT NULL THEN c_observation_id END AS c_observation_duration_id,
  c.c_active_start::timestamp + (c.c_active_end::timestamp - c.c_active_start::timestamp) * 0.5 AS c_reference_time
  FROM t_observation o
  LEFT JOIN t_proposal p on p.c_program_id = o.c_program_id
  LEFT JOIN t_cfp c on p.c_cfp_id = c.c_cfp_id
;

