-- Truncate to allow schema change (c_asterism_results was NOT NULL).
TRUNCATE TABLE t_itc_result;

-- Make c_asterism_results nullable and add c_error_message for deterministic failures.
-- Exactly one of the two must be non-null (success vs. cached deterministic failure).
ALTER TABLE t_itc_result
  ALTER COLUMN c_asterism_results DROP NOT NULL,
  ADD COLUMN c_error_message text,
  ADD CONSTRAINT t_itc_result_one_result CHECK (num_nonnulls(c_asterism_results, c_error_message) = 1);

-- When the ITC version changes, reset obscalc
CREATE OR REPLACE FUNCTION itc_version_update()
  RETURNS trigger AS $$
BEGIN
  NEW.c_last_update = NOW();
  IF (OLD.c_version IS DISTINCT FROM NEW.c_version OR OLD.c_data IS DISTINCT FROM NEW.c_data) THEN
    TRUNCATE t_itc_result;

    -- Reset to pending. Inactive, ongoing, completed and calculating are left alone.
    UPDATE t_obscalc SET
      c_last_invalidation = NOW(),
      c_failure_count     = 0,
      c_retry_at          = NULL,
      c_obscalc_state     = 'pending'
    WHERE c_obscalc_state IN ('ready', 'retry')
      AND c_workflow_state NOT IN ('inactive', 'ongoing', 'completed');
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;
