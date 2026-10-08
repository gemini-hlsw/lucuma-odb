-- The exposure rules that each sequence violates, as (severity, description)
-- pairs, found in the same pass over the sequence that computes its digest and
-- kept with the rest of the acquisition and science digests: in the obscalc
-- result and in the digest cache.  The cache is keyed by the generator hash,
-- which now includes the observation's constraints, so a workflow computed
-- without generating the sequence (e.g., when checking a requested state
-- transition) reads violations that match the current inputs.  Every cached
-- digest's hash changes with this release, so the default never stands in for
-- a digest that was not checked.
--
-- In t_obscalc the digest is read as a whole, all of its columns null or none,
-- so the new columns are null exactly when the rest of the digest is.  An
-- existing digest gets an empty placeholder, which the recalculation below
-- replaces wherever it matters.
ALTER TABLE t_obscalc
  ADD COLUMN c_acq_exposure_time_violations jsonb NULL,
  ADD COLUMN c_sci_exposure_time_violations jsonb NULL;

UPDATE t_obscalc
   SET c_acq_exposure_time_violations = '[]'::jsonb,
       c_sci_exposure_time_violations = '[]'::jsonb
 WHERE c_acq_execution_state IS NOT NULL;

ALTER TABLE t_execution_digest
  ADD COLUMN c_acq_exposure_time_violations jsonb NOT NULL DEFAULT '[]'::jsonb,
  ADD COLUMN c_sci_exposure_time_violations jsonb NOT NULL DEFAULT '[]'::jsonb;

-- Recalculate everything that might have an exposure time violation and where
-- it would matter: observations with a sequence that are not finished.
-- Undefined observations are skipped, since they cannot be generated, and
-- completed ones are never checked again.  Inactive ones keep the placeholder
-- until reactivated: changing the workflow user state edits t_observation,
-- whose trigger recalculates the observation, and Inactive can only return to
-- its validation state, never directly to Ready.
UPDATE t_obscalc SET
  c_last_invalidation = NOW(),
  c_failure_count     = 0,
  c_retry_at          = NULL,
  c_obscalc_state     = 'pending'
WHERE c_workflow_state IN ('unapproved', 'defined', 'ready', 'ongoing');

-- The digest cache is keyed by the generator inputs, which do not include a
-- materialized sequence, so replacing or resetting the sequence must clear the
-- observation's cached digest, as completing a step already does.
CREATE OR REPLACE FUNCTION delete_execution_digest_for_materialization()
  RETURNS TRIGGER AS $$
BEGIN
  IF TG_OP = 'DELETE' THEN
    DELETE FROM t_execution_digest WHERE c_observation_id = OLD.c_observation_id;
  ELSE
    DELETE FROM t_execution_digest WHERE c_observation_id = NEW.c_observation_id;
  END IF;
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER delete_execution_digest_on_materialization_trigger
  AFTER INSERT OR UPDATE OR DELETE ON t_sequence_materialization
  FOR EACH ROW EXECUTE FUNCTION delete_execution_digest_for_materialization();
