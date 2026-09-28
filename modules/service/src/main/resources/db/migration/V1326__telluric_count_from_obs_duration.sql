-- The number of tellurics a science observation gets (one, or two above the
-- multi-telluric threshold) now follows its observation duration, so editing
-- the duration must re-run the calibration calculation.

CREATE OR REPLACE FUNCTION cascade_calibration_duration_recalc()
RETURNS TRIGGER AS $$
BEGIN
  IF NEW.c_observation_duration IS DISTINCT FROM OLD.c_observation_duration
     AND NEW.c_calibration_role IS NULL THEN
    CALL invalidate_calibration_calc(NEW.c_observation_id, NEW.c_program_id, 'recalc');
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER cascade_calibration_duration_recalc_trigger
  AFTER UPDATE OF c_observation_duration ON t_observation
  FOR EACH ROW
  EXECUTE FUNCTION cascade_calibration_duration_recalc();
