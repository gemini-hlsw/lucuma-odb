-- Durable calibration-recalculation queue: rows survive calibrations-daemon
-- downtime and are replayed on restart. Modeled on t_telluric_resolution
-- (V1067) and cascade_telluric_invalidation (V1072).

-- One row per science observation whose obscalc has settled and whose program's
-- calibrations may need recalculation.
CREATE TABLE t_calibration_calc (
  c_observation_id     d_observation_id  NOT NULL PRIMARY KEY,

  -- Delete if the parent observation gets deleted.
  FOREIGN KEY (c_observation_id)
    REFERENCES t_observation(c_observation_id) ON DELETE CASCADE,

  c_program_id         d_program_id      NOT NULL,

  -- current state, starts as pending
  c_state              e_calculation_state NOT NULL DEFAULT 'pending',

  -- When the entry was last marked dirty. If it changed while a worker was
  -- calculating, the row goes back to 'pending' instead of 'ready'.
  c_last_invalidation  TIMESTAMP         NOT NULL DEFAULT now(),

  -- When the last result was written.
  c_last_update        TIMESTAMP         NOT NULL DEFAULT now(),

  -- If in 'retry' state, the retry will not be attempted before this time.
  c_retry_at           TIMESTAMP         NULL DEFAULT NULL,

  -- Failed attempts since the entry was last 'pending'.
  c_failure_count      INT4              NOT NULL DEFAULT 0 CHECK (c_failure_count >= 0),

  -- Error message from the most recent failed attempt, if any.
  c_error_message      TEXT              NULL,

  -- retry fields only apply to 'retry' and 'calculating'
  CONSTRAINT check_retry_fields_only_for_retry_state CHECK (
    (c_state = 'retry' OR c_state = 'calculating') OR
    (c_retry_at IS NULL AND c_failure_count = 0)
  ),

  -- when in a retry state, there must be a retry at time
  CONSTRAINT check_retry_at_defined_for_retry_state CHECK (
    (c_state != 'retry') OR (c_retry_at IS NOT NULL)
  )
);

CREATE INDEX i_calibration_calc_state   ON t_calibration_calc (c_state, c_last_invalidation);
CREATE INDEX i_calibration_calc_program ON t_calibration_calc (c_program_id);

-- Enqueue a calibration recalculation, mirroring invalidate_telluric_resolution.
-- A 'calculating' row keeps its state (the worker notices the bumped
-- c_last_invalidation and falls back to 'pending'); anything else goes 'pending'.
CREATE PROCEDURE invalidate_calibration_calc(
  science_obs_id d_observation_id,
  prog_id        d_program_id
) LANGUAGE plpgsql AS $$
DECLARE
  current_state e_calculation_state;
BEGIN
  INSERT INTO t_calibration_calc (c_observation_id, c_program_id)
  VALUES (science_obs_id, prog_id)
  ON CONFLICT (c_observation_id) DO NOTHING;

  SELECT c_state INTO current_state
  FROM t_calibration_calc
  WHERE c_observation_id = science_obs_id
  FOR UPDATE;

  IF current_state = 'calculating' THEN
    UPDATE t_calibration_calc
    SET c_last_invalidation = now(),
        c_failure_count     = 0,
        c_retry_at          = NULL
    WHERE c_observation_id = science_obs_id;
  ELSE
    UPDATE t_calibration_calc
    SET c_last_invalidation = now(),
        c_failure_count     = 0,
        c_retry_at          = NULL,
        c_state             = 'pending'
    WHERE c_observation_id = science_obs_id;
  END IF;
END;
$$;

-- Enqueue a recalculation when a non-calibration observation's obscalc settles
-- to 'ready'. The c_calibration_role guard keeps calibration observations out of
-- the queue, so the daemon cannot recurse on its own output.
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
    CALL invalidate_calibration_calc(NEW.c_observation_id, NEW.c_program_id);
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER cascade_calibration_invalidation_trigger
  AFTER UPDATE OF c_last_update ON t_obscalc
  FOR EACH ROW
  EXECUTE FUNCTION cascade_calibration_invalidation();

-- Notify on INSERT or on any transition to 'pending'. Payload:
-- observation_id, program_id, old state (or 'null'), new state.
CREATE OR REPLACE FUNCTION ch_calibration_calc_edit()
  RETURNS trigger AS $$
BEGIN
  IF (TG_OP = 'INSERT' OR NEW.c_state = 'pending') THEN
    PERFORM pg_notify(
      'ch_calibration_calc',
      NEW.c_observation_id::text || ',' ||
      NEW.c_program_id::text || ',' ||
      COALESCE(OLD.c_state::text, 'null') || ',' ||
      NEW.c_state::text
    );
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER ch_calibration_calc_trigger
  AFTER INSERT OR UPDATE ON t_calibration_calc
  FOR EACH ROW
  EXECUTE FUNCTION ch_calibration_calc_edit();
