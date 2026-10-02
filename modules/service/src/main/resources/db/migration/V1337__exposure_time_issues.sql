-- Exposure times outside an instrument's limits, found by obscalc as it walks
-- the sequence.  They are stored so that a workflow computed without
-- generating the sequence (e.g., when checking a requested state transition)
-- sees the same issues as obscalc did.
ALTER TABLE t_obscalc
  ADD COLUMN c_exposure_time_issues jsonb NOT NULL DEFAULT '[]'::jsonb;

-- Recalculate everything that might have an exposure time issue and where it
-- would matter: observations with a sequence that are not finished.  Undefined
-- observations are skipped, since they cannot be generated.
UPDATE t_obscalc SET
  c_last_invalidation = NOW(),
  c_failure_count     = 0,
  c_retry_at          = NULL,
  c_obscalc_state     = 'pending'
WHERE c_workflow_state IN ('unapproved', 'defined', 'ready', 'ongoing');
