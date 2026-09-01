-- When the ITC version changes, every non-frozen cached ITC result is deleted.
-- V1208 narrowed the accompanying obscalc invalidation to the 'ready' workflow
-- state (plus observations already carrying an ITC error) in order to bound the
-- recalculation burst.  That left 'defined' and 'unapproved' observations with
-- a purged ITC result and an obscalc row still marked 'ready', and nothing to
-- requeue them.  Any later live workflow computation -- setProposalStatus,
-- AccessControl.filterState -- consults the ITC cache only, so it reads the
-- missing result as an ITC failure and calls the observation undefined, while
-- the UI keeps showing the stale 'defined' state.
--
-- Invalidate obscalc for exactly the observations whose cached result was
-- actually removed.  Frozen results survive the delete, so their observations
-- are correctly left alone.
CREATE OR REPLACE FUNCTION itc_version_update()
  RETURNS trigger AS $$
BEGIN
  NEW.c_last_update = NOW();
  IF (OLD.c_version IS DISTINCT FROM NEW.c_version OR OLD.c_data IS DISTINCT FROM NEW.c_data) THEN

    WITH deleted AS (
      DELETE FROM t_itc_result
       WHERE NOT c_is_frozen
      RETURNING c_program_id, c_observation_id
    )
    UPDATE t_obscalc o SET
      c_last_invalidation = NOW(),
      c_failure_count     = 0,
      c_retry_at          = NULL,
      c_obscalc_state     = 'pending'
    FROM deleted d
    WHERE o.c_program_id     = d.c_program_id
      AND o.c_observation_id = d.c_observation_id
      AND o.c_obscalc_state IN ('ready', 'retry')
      AND o.c_workflow_state NOT IN ('inactive', 'ongoing', 'completed');

    -- Observations that recorded an ITC error in their workflow but had no
    -- cached row of their own to delete (kept from V1208).
    UPDATE t_obscalc SET
      c_last_invalidation = NOW(),
      c_failure_count     = 0,
      c_retry_at          = NULL,
      c_obscalc_state     = 'pending'
    WHERE c_obscalc_state IN ('ready', 'retry')
      AND c_workflow_state NOT IN ('inactive', 'ongoing', 'completed')
      AND c_workflow_validations @> '[{"code": "ITC_ERROR"}]'::jsonb;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- V1299 dropped and recreated e_workflow_state, giving it a new OID.  When
-- Flyway applies V1299 and this migration in the same session as an earlier
-- migration that already wrote to t_obscalc (V1273 among others), plpgsql is
-- still holding a compiled ch_obscalc_update() whose plan casts
-- c_workflow_state using the dropped type, and the UPDATE below dies with
-- "cache lookup failed for type <oid>".  Touching the pg_proc row forces a
-- recompile.  Harmless when the plan was never cached.
ALTER FUNCTION ch_obscalc_update() RESET ALL;

-- Repair the rows already stranded by the previous version of the trigger:
-- obscalc says the calculation had an ITC result, but no such result remains.
UPDATE t_obscalc o SET
  c_last_invalidation = NOW(),
  c_failure_count     = 0,
  c_retry_at          = NULL,
  c_obscalc_state     = 'pending'
WHERE o.c_obscalc_state IN ('ready', 'retry')
  AND o.c_workflow_state NOT IN ('inactive', 'ongoing', 'completed')
  AND o.c_has_itc_result
  AND NOT EXISTS (
        SELECT 1 FROM t_itc_result r
         WHERE r.c_program_id      = o.c_program_id
           AND r.c_observation_id  = o.c_observation_id
           AND r.c_science_results IS NOT NULL
      );
