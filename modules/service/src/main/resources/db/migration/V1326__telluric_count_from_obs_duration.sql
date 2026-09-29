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

-- A telluric with a visit is spent: its star is part of what was observed, so
-- an invalidation of the science must not search for it again. Each row keeps
-- its own state, rather than all following the first row read.
CREATE OR REPLACE PROCEDURE invalidate_telluric_resolution(
  science_obs_id d_observation_id
) LANGUAGE plpgsql AS $$
BEGIN
  UPDATE t_telluric_resolution r
  SET    c_last_invalidation = now(),
         c_failure_count     = 0,
         c_retry_at          = NULL,
         c_state             = CASE WHEN r.c_state = 'calculating' THEN r.c_state
                                    ELSE 'pending'::e_calculation_state END
  WHERE  r.c_science_observation_id = science_obs_id
    AND  NOT EXISTS (SELECT 1 FROM t_visit v WHERE v.c_observation_id = r.c_observation_id);
END;
$$;
