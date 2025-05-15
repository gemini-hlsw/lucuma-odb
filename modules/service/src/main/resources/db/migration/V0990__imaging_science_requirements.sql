-- Add imaging science requirements tables
ALTER TABLE t_observation
  ADD COLUMN c_img_minimum_fov d_angle_µas null default null,
  ADD COLUMN c_img_narrow_filters boolean null default null,
  ADD COLUMN c_img_broad_filters boolean null default null;

-- Rename spec constraint
-- the exposure time mode is shared between imaging and spectroscopy
ALTER TABLE t_observation RENAME COLUMN c_spec_exp_time_mode TO c_exp_time_mode;
ALTER TABLE t_observation RENAME COLUMN c_spec_signal_to_noise TO c_etm_signal_to_noise;
ALTER TABLE t_observation RENAME COLUMN c_spec_signal_to_noise_at TO c_etm_signal_to_noise_at;
ALTER TABLE t_observation RENAME COLUMN c_spec_exp_time TO c_etm_exp_time;
ALTER TABLE t_observation RENAME COLUMN c_spec_exp_count TO c_etm_exp_count;

-- GMOS North imaging requirements
CREATE TABLE t_imaging_gmos_north (
  c_observation_id d_observation_id PRIMARY KEY REFERENCES t_observation(c_observation_id),
  c_filter         d_tag            NOT NULL REFERENCES    t_gmos_north_filter(c_tag),
  UNIQUE (c_observation_id, c_filter)
);
COMMENT ON TABLE t_imaging_gmos_north IS 'GMOS North imaging requirements';

-- GMOS South imaging requirements
CREATE TABLE t_imaging_gmos_south (
  c_observation_id d_observation_id PRIMARY KEY REFERENCES t_observation(c_observation_id),
  c_filter         d_tag            NOT NULL REFERENCES    t_gmos_south_filter(c_tag),
  UNIQUE (c_observation_id, c_filter)
);
COMMENT ON TABLE t_imaging_gmos_south IS 'GMOS South imaging requirements';

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
  CASE WHEN o.c_observation_duration     IS NOT NULL THEN o.c_observation_id END AS c_observation_duration_id,
  CASE WHEN o.c_img_minimum_fov          IS NOT NULL THEN o.c_observation_id END AS c_img_minimum_fov_id,
  CASE WHEN o.c_img_narrow_filters       IS NOT NULL THEN o.c_observation_id END AS c_img_narrow_filters_id,
  CASE WHEN o.c_img_broad_filters        IS NOT NULL THEN o.c_observation_id END AS c_img_broad_filters_id,
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

CREATE OR REPLACE FUNCTION ch_observation_edit_img_requirements()
  RETURNS trigger AS $$
DECLARE
  observation record;
  program_id d_program_id;
BEGIN
  observation := COALESCE(NEW, OLD);
  program_id := (SELECT c_program_id FROM t_observation WHERE c_observation_id = observation.c_observation_id);
  IF ROW(NEW.*) IS DISTINCT FROM ROW(OLD.*) THEN
    PERFORM pg_notify('ch_observation_edit', observation.c_observation_id || ',' || program_id  || ',' || 'UPDATE');
  END IF;
  RETURN observation;
END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER ch_observation_edit_img_gmos_north
  AFTER INSERT OR UPDATE OR DELETE ON t_imaging_gmos_north
  DEFERRABLE
  FOR EACH ROW
  EXECUTE PROCEDURE ch_observation_edit_img_requirements();

CREATE CONSTRAINT TRIGGER ch_observation_edit_img_gmos_south
  AFTER INSERT OR UPDATE OR DELETE ON t_imaging_gmos_south
  DEFERRABLE
  FOR EACH ROW
  EXECUTE PROCEDURE ch_observation_edit_img_requirements();

