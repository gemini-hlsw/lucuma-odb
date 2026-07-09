-- When the ITC version changes, reset obscalc. Previously this reset every
-- workflow state except inactive, ongoing and completed. Limit it to the
-- 'ready' workflow state only.
CREATE OR REPLACE FUNCTION itc_version_update()
  RETURNS trigger AS $$
BEGIN
  NEW.c_last_update = NOW();
  IF (OLD.c_version IS DISTINCT FROM NEW.c_version OR OLD.c_data IS DISTINCT FROM NEW.c_data) THEN
    TRUNCATE t_itc_result;

    -- Reset to pending, but only for observations in the 'ready' workflow state.
    UPDATE t_obscalc SET
      c_last_invalidation = NOW(),
      c_failure_count     = 0,
      c_retry_at          = NULL,
      c_obscalc_state     = 'pending'
    WHERE c_obscalc_state IN ('ready', 'retry')
      AND c_workflow_state = 'ready';
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;
