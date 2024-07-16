-- Notify of obs time changes for calibrations
CREATE OR REPLACE FUNCTION ch_calib_obs_time()
RETURNS TRIGGER AS $$
DECLARE
  observation record;
BEGIN
    observation := COALESCE(NEW, OLD);
    IF NEW.c_observation_time <> OLD.c_observation_time AND NEW.c_calibration_role IS NOT NULL THEN
      PERFORM pg_notify('ch_calib_obs_time',  observation.c_program_id || ',' || observation.c_observation_id || ',' || 'UPDATE');
    END IF;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER ch_obs_viz_time_trigger
  AFTER UPDATE OF c_observation_time ON t_observation
  FOR EACH ROW
  EXECUTE PROCEDURE ch_calib_obs_time();

-- Generate the view for the generator including observation_time
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
  o.c_spec_signal_to_noise,
  o.c_spec_signal_to_noise_at,
  o.c_observing_mode_type,
  o.c_observation_time,
  t.c_target_id,
  t.c_sid_rv,
  t.c_source_profile
FROM
  t_observation o
LEFT JOIN t_asterism_target a
  ON o.c_observation_id = a.c_observation_id
LEFT JOIN t_target t
  ON a.c_target_id      = t.c_target_id
ORDER BY
  o.c_observation_id,
  t.c_target_id;
