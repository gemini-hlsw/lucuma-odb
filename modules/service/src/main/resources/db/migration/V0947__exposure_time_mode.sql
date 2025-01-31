-- Add fixed exposure time mode columns.
CREATE TYPE e_exp_time_mode AS ENUM (
  -- two more modes to come one day (signal_and_time, signal_and_count)
  'signal_to_noise',
  'time_and_count'
);
COMMENT ON TYPE e_exp_time_mode IS 'Exposure time mode options';

ALTER TABLE t_observation
  ADD COLUMN c_spec_exp_time_mode e_exp_time_mode null default null,
  ADD COLUMN c_spec_exp_time  interval null default null check (c_spec_exp_time  >= interval '0 seconds'),
  ADD COLUMN c_spec_exp_count integer  null default null check (c_spec_exp_count >= 0);

-- Check that the exposure mode options are valid.  Either one of S/N or fixed
-- may be specified but not both.  If either is specified it must be fully
-- specified.
ALTER TABLE t_observation
  ADD CONSTRAINT exp_time_mode_check CHECK (
    CASE c_spec_exp_time_mode
      WHEN 'signal_to_noise' THEN num_nulls(c_spec_signal_to_noise_at, c_spec_signal_to_noise)            = 0
      WHEN 'time_and_count'  THEN num_nulls(c_spec_signal_to_noise_at, c_spec_exp_count, c_spec_exp_time) = 0
      ELSE TRUE
    END
  );

-- Replace the observation view to add the fixed exposure mode columns.
DROP VIEW v_observation;

CREATE OR REPLACE VIEW v_observation AS
  SELECT o.*,
  CASE WHEN o.c_explicit_ra              IS NOT NULL THEN o.c_observation_id END AS c_explicit_base_id,
  CASE WHEN o.c_air_mass_min             IS NOT NULL THEN o.c_observation_id END AS c_air_mass_id,
  CASE WHEN o.c_hour_angle_min           IS NOT NULL THEN o.c_observation_id END AS c_hour_angle_id,
  CASE WHEN o.c_observing_mode_type      IS NOT NULL THEN o.c_observation_id END AS c_observing_mode_id,
  CASE WHEN o.c_spec_wavelength          IS NOT NULL THEN o.c_observation_id END AS c_spec_wavelength_id,

  CASE WHEN o.c_spec_exp_time_mode       IS NOT NULL THEN o.c_observation_id END AS c_spec_exp_time_mode_id,
  CASE WHEN o.c_spec_exp_time_mode = 'signal_to_noise' THEN o.c_observation_id END AS c_spec_signal_to_noise_id,
  CASE WHEN o.c_spec_exp_time_mode = 'time_and_count'  THEN o.c_observation_id END AS c_spec_time_and_count_id,

  CASE WHEN o.c_spec_wavelength_coverage IS NOT NULL THEN o.c_observation_id END AS c_spec_wavelength_coverage_id,
  CASE WHEN o.c_spec_focal_plane_angle   IS NOT NULL THEN o.c_observation_id END AS c_spec_focal_plane_angle_id,
  CASE WHEN o.c_observation_duration     IS NOT NULL THEN o.c_observation_id END AS c_observation_duration_id,
  c.c_active_start::timestamp + (c.c_active_end::timestamp - c.c_active_start::timestamp) * 0.5 AS c_reference_time
  FROM t_observation o
  LEFT JOIN t_proposal p on p.c_program_id = o.c_program_id
  LEFT JOIN t_cfp c on p.c_cfp_id = c.c_cfp_id
  LEFT JOIN t_gmos_north_long_slit mode_gnls ON o.c_observation_id = mode_gnls.c_observation_id
  LEFT JOIN t_gmos_south_long_slit mode_gsls ON o.c_observation_id = mode_gsls.c_observation_id;

DROP VIEW v_generator_params;

-- Add exposure time mode columns
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
  o.c_spec_exp_time_mode,
  o.c_spec_signal_to_noise,
  o.c_spec_signal_to_noise_at,
  o.c_spec_exp_time,
  o.c_spec_exp_count,
  o.c_observing_mode_type,
  o.c_science_band,
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