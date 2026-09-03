-- Extend the durable calibration queue (V1295) to also carry
-- calibration-target retargeting work, replacing the fire-and-forget
-- ch_calib_obs_time NOTIFY (V0905). Science and calibration observation ids
-- are disjoint, so one row per observation id still holds.

CREATE TYPE e_calibration_work_type AS ENUM ('recalc', 'retarget');

ALTER TABLE t_calibration_calc
  ADD COLUMN c_work_type e_calibration_work_type NOT NULL DEFAULT 'recalc';

DROP PROCEDURE invalidate_calibration_calc(d_observation_id, d_program_id);

CREATE PROCEDURE invalidate_calibration_calc(
  obs_id    d_observation_id,
  prog_id   d_program_id,
  work_type e_calibration_work_type
) LANGUAGE plpgsql AS $$
DECLARE
  current_state e_calculation_state;
BEGIN
  -- The work type never changes for a given id (science and calibration ids
  -- are disjoint), so the conflict branch leaves it alone.
  INSERT INTO t_calibration_calc (c_observation_id, c_program_id, c_work_type)
  VALUES (obs_id, prog_id, work_type)
  ON CONFLICT (c_observation_id) DO NOTHING;

  SELECT c_state INTO current_state
  FROM t_calibration_calc
  WHERE c_observation_id = obs_id
  FOR UPDATE;

  IF current_state = 'calculating' THEN
    UPDATE t_calibration_calc
    SET c_last_invalidation = now(),
        c_failure_count     = 0,
        c_retry_at          = NULL
    WHERE c_observation_id = obs_id;
  ELSE
    UPDATE t_calibration_calc
    SET c_last_invalidation = now(),
        c_failure_count     = 0,
        c_retry_at          = NULL,
        c_state             = 'pending'
    WHERE c_observation_id = obs_id;
  END IF;
END;
$$;

CREATE OR REPLACE FUNCTION cascade_calibration_invalidation()
RETURNS TRIGGER AS $$
BEGIN
  IF NEW.c_last_update IS DISTINCT FROM OLD.c_last_update
     AND NEW.c_obscalc_state = 'ready'
     AND NOT EXISTS (
       SELECT 1 FROM t_observation
       WHERE c_observation_id = NEW.c_observation_id
         AND c_calibration_role IS NOT NULL
     ) THEN
    CALL invalidate_calibration_calc(NEW.c_observation_id, NEW.c_program_id, 'recalc');
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Enqueue a retarget when a calibration observation's time changes.
CREATE OR REPLACE FUNCTION cascade_calibration_retarget()
RETURNS TRIGGER AS $$
BEGIN
  IF NEW.c_observation_time IS DISTINCT FROM OLD.c_observation_time
     AND NEW.c_calibration_role IS NOT NULL THEN
    CALL invalidate_calibration_calc(NEW.c_observation_id, NEW.c_program_id, 'retarget');
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER cascade_calibration_retarget_trigger
  AFTER UPDATE OF c_observation_time ON t_observation
  FOR EACH ROW
  EXECUTE FUNCTION cascade_calibration_retarget();

-- The durable queue replaces the NOTIFY-only path.
DROP TRIGGER ch_obs_viz_time_trigger ON t_observation;
DROP FUNCTION ch_calib_obs_time();
