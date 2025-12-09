
CREATE FUNCTION is_telluric_calibration(obs_id d_observation_id)
RETURNS BOOLEAN AS $$
BEGIN
  RETURN EXISTS (
    SELECT 1 FROM t_observation
    WHERE c_observation_id = obs_id
      AND c_calibration_role = 'telluric'
  );
END;
$$ LANGUAGE plpgsql STABLE;

-- Telluric target tracking (closely based on obscalc)
CREATE TABLE t_telluric_resolution (
  c_observation_id         d_observation_id  NOT NULL PRIMARY KEY,

  -- Delete if the parent gets deleted
  FOREIGN KEY (c_observation_id)
    REFERENCES t_observation(c_observation_id)
    ON DELETE CASCADE,

  -- the observation is a telluric calibration
  CONSTRAINT check_observation_is_telluric CHECK (
    is_telluric_calibration(c_observation_id)
  ),

  c_program_id             d_program_id      NOT NULL,
  c_science_observation_id d_observation_id  NOT NULL,

  -- current state, starts as pending
  c_state                  e_calculation_state NOT NULL DEFAULT 'pending',

  -- When the entry was last marked dirty
  c_last_invalidation      TIMESTAMP         NOT NULL DEFAULT now(),

  -- When the last result was written
  c_last_update            TIMESTAMP         NOT NULL DEFAULT now(),

  -- Retry tracking
  c_retry_at               TIMESTAMP         NULL DEFAULT NULL,
  c_failure_count          INTEGER           NOT NULL DEFAULT 0
    CHECK (c_failure_count >= 0),

  -- Result: resolved target
  c_resolved_target_id     d_target_id       NULL,

  -- Error message (if permanently failed)
  c_error_message          TEXT              NULL,

  CONSTRAINT check_retry_fields_only_for_retry_state CHECK (
    (c_state = 'retry' OR c_state = 'calculating') OR
    (c_retry_at IS NULL AND c_failure_count = 0)
  ),

  CONSTRAINT check_retry_at_defined_for_retry_state CHECK (
    (c_state != 'retry') OR (c_retry_at IS NOT NULL)
  )
);

-- Function to publish telluric events
CREATE OR REPLACE FUNCTION ch_telluric_resolution_edit()
  RETURNS trigger AS $$
BEGIN
  -- Notify on INSERT or UPDATE to 'pending' state
  IF (TG_OP = 'INSERT' OR NEW.c_state = 'pending') THEN
    PERFORM pg_notify(
      'ch_telluric_resolution',
      NEW.c_observation_id::text || ',' ||
      NEW.c_program_id::text || ',' ||
      COALESCE(OLD.c_state::text, 'null') || ',' ||
      NEW.c_state::text
    );
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER ch_telluric_resolution_trigger
  AFTER INSERT OR UPDATE ON t_telluric_resolution
  FOR EACH ROW
  EXECUTE FUNCTION ch_telluric_resolution_edit();

-- Modify the calibration target constraint to allow telluric observations without targets
CREATE OR REPLACE FUNCTION validate_calibration_has_target()
RETURNS TRIGGER AS $$
DECLARE
  target_count INTEGER;
  current_existence e_existence;
  current_calib_role e_calibration_role;
BEGIN
  -- This handles cases where the observation might be deleted after the update
  SELECT c_existence, c_calibration_role
  INTO current_existence, current_calib_role
  FROM t_observation
  WHERE c_program_id = NEW.c_program_id
    AND c_observation_id = NEW.c_observation_id;

  IF NOT FOUND OR current_existence = 'deleted' THEN
    RETURN NEW;
  END IF;

  -- check for twilight, spectrophotometric, and photometric calibrations
  IF current_calib_role IN ('twilight', 'spectrophotometric', 'photometric') THEN

    SELECT COUNT(*) INTO target_count
    FROM t_asterism_target
    WHERE c_program_id = NEW.c_program_id
      AND c_observation_id = NEW.c_observation_id;

    -- Require at least one target
    IF target_count = 0 THEN
      RAISE EXCEPTION '% calibration observations must have at least one target (observation: %)',
        current_calib_role, NEW.c_observation_id;
    END IF;
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;
