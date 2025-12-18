-- Cascade obscalc invalidation to telluric resolution
-- will request a new target update when obscalc updates

CREATE PROCEDURE invalidate_telluric_resolution(
  science_obs_id d_observation_id
) LANGUAGE plpgsql AS $$
DECLARE
  telluric_state e_calculation_state;
BEGIN
  SELECT c_state INTO telluric_state
  FROM t_telluric_resolution
  WHERE c_science_observation_id = science_obs_id
  FOR UPDATE;

  IF NOT FOUND THEN
    RETURN;
  END IF;

  IF telluric_state = 'calculating' THEN
    UPDATE t_telluric_resolution
    SET c_last_invalidation = now(),
        c_failure_count = 0,
        c_retry_at = NULL
    WHERE c_science_observation_id = science_obs_id;
  ELSE
    UPDATE t_telluric_resolution
    SET c_last_invalidation = now(),
        c_failure_count = 0,
        c_retry_at = NULL,
        c_state = 'pending'
    WHERE c_science_observation_id = science_obs_id;
  END IF;
END;
$$;

-- Listen to c_last_update obscalc changes, some of those will need to invalidate
-- telluric target. Only for science observations (not calibrations).
CREATE OR REPLACE FUNCTION cascade_telluric_invalidation()
RETURNS TRIGGER AS $$
BEGIN
  IF NEW.c_last_update IS DISTINCT FROM OLD.c_last_update
     AND NEW.c_workflow_state IN ('ready', 'defined')
     AND NOT EXISTS (
       SELECT 1 FROM t_observation
       WHERE c_observation_id = NEW.c_observation_id
         AND c_calibration_role IS NOT NULL
     ) THEN
    CALL invalidate_telluric_resolution(NEW.c_observation_id);
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER cascade_telluric_invalidation_trigger
  AFTER UPDATE OF c_last_update ON t_obscalc
  FOR EACH ROW
  EXECUTE FUNCTION cascade_telluric_invalidation();

-- Store hash of telluric search parameters to detect changes
ALTER TABLE t_telluric_resolution
ADD COLUMN c_params_hash bytea NULL;
