-- Telluric Target Resolution

-- Telluric target tracking based on obscalc
CREATE TABLE t_telluric_resolution (
  -- Primary key: the telluric observation
  c_observation_id         d_observation_id  NOT NULL PRIMARY KEY,

  FOREIGN KEY (c_observation_id)
    REFERENCES t_observation(c_observation_id)
    ON DELETE CASCADE,

  -- Foreign keys
  c_program_id             d_program_id      NOT NULL,
  c_science_observation_id d_observation_id  NOT NULL,

  -- current state, starts as pending
  c_state                  e_calculation_state NOT NULL DEFAULT 'pending',

  -- When the entry was last marked dirty
  c_last_invalidation      TIMESTAMP         NOT NULL DEFAULT now(),

  -- When the last result was written
  c_last_update            TIMESTAMP         NOT NULL DEFAULT now(),

  -- Retry tracking (exponential backoff)
  c_retry_at               TIMESTAMP         NULL DEFAULT NULL,
  c_failure_count          INTEGER           NOT NULL DEFAULT 0
    CHECK (c_failure_count >= 0),

  -- Result: resolved target (if successful)
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

-- Function to publish resolution events
CREATE OR REPLACE FUNCTION ch_telluric_resolution_edit()
  RETURNS trigger AS $$
BEGIN
  -- Notify on INSERT or UPDATE to 'pending' state
  IF (TG_OP = 'INSERT' OR NEW.c_state = 'pending') THEN
    PERFORM pg_notify(
      'ch_telluric_resolution',
      NEW.c_observation_id::text || '|' ||
      NEW.c_program_id::text || '|' ||
      COALESCE(OLD.c_state::text, 'null') || '|' ||
      NEW.c_state::text
    );
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Trigger on t_telluric_resolution
CREATE TRIGGER ch_telluric_resolution_trigger
  AFTER INSERT OR UPDATE ON t_telluric_resolution
  FOR EACH ROW
  EXECUTE FUNCTION ch_telluric_resolution_edit();
