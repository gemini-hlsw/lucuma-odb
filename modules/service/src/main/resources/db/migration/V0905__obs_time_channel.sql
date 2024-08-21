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
