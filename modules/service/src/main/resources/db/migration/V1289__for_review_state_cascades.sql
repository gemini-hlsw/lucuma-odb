-- ObservationWorkflowState.ForReview (V1281, V1285) was never added to the two
-- trigger functions that test c_workflow_state against literal state names, so
-- an observation sitting in 'for_review' silently dropped out of both cascades:
--
--   * cascade_telluric_invalidation() (V1072) stopped re-requesting its telluric
--     target when obscalc changed, leaving a stale resolution;
--   * itc_version_update() (V1225) stopped resetting it to pending on an ITC
--     version bump, leaving stale ITC results behind a new ITC version.
--
-- Nothing checks these literals against the Scala enum -- StartupDiagnostics
-- compares e_workflow_state itself, not the plpgsql that reads it -- so both are
-- repaired here, each with a note about what the next state added has to do.

-- 'for_review' belongs with 'defined' and 'ready': the observation is still a
-- live pre-execution candidate and still wants a current telluric.  Written as
-- the complement of the states that are *not* candidates so that a state added
-- later is picked up by default; the failure modes are asymmetric, since
-- over-including costs one redundant telluric recalculation while
-- under-including is the stale resolution being fixed here.  The surviving set
-- today is exactly ('defined', 'for_review', 'ready').
CREATE OR REPLACE FUNCTION cascade_telluric_invalidation()
RETURNS TRIGGER AS $$
BEGIN
  IF NEW.c_last_update IS DISTINCT FROM OLD.c_last_update
     AND NEW.c_workflow_state NOT IN ('inactive', 'undefined', 'unapproved', 'ongoing', 'completed')
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

-- Here the set stays an explicit inclusion list.  V1208 deliberately narrowed
-- this from "everything not executed" to 'ready' alone, so 'defined' is excluded
-- on purpose and a future state must not join by default: add it below only if
-- an observation in that state should be re-costed when the ITC version moves.
CREATE OR REPLACE FUNCTION itc_version_update()
  RETURNS trigger AS $$
BEGIN
  NEW.c_last_update = NOW();
  IF (OLD.c_version IS DISTINCT FROM NEW.c_version OR OLD.c_data IS DISTINCT FROM NEW.c_data) THEN
    DELETE FROM t_itc_result WHERE NOT c_is_frozen;

    -- Reset to pending, but only for observations in a committed workflow state
    -- ('ready' or 'for_review') or non-executed ones with itc errors.
    UPDATE t_obscalc SET
      c_last_invalidation = NOW(),
      c_failure_count     = 0,
      c_retry_at          = NULL,
      c_obscalc_state     = 'pending'
    WHERE c_obscalc_state IN ('ready', 'retry')
      AND c_workflow_state NOT IN ('inactive', 'ongoing', 'completed')
      AND (
            c_workflow_state IN ('ready', 'for_review')
            OR c_workflow_validations @> '[{"code": "ITC_ERROR"}]'::jsonb
          );
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;
