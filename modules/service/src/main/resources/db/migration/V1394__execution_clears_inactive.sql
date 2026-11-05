-- An observation is inactive while executing only because staff suspended it.
--
-- WHY
--
-- The workflow lets an Inactive user state override an Ongoing execution state,
-- so that staff can suspend an observation part way through.  Only staff may
-- set an Ongoing observation Inactive, but the user state column does not say
-- who set it, or when.  Without help, an Inactive stored before execution (a PI
-- deactivating an observation the observatory then ran anyway) would override
-- execution exactly as a suspension does.
--
-- Nor is there any use for an Inactive user state on a Completed observation.
-- Completed wins in the workflow, so it is invisible there, but every query that
-- filters on the raw user state (time estimates, configuration requests, the
-- proposal summary, calibration generation) still treats the observation as
-- inactive, and Completed offers no transition that would clear it.
--
-- WHAT
--
-- So the user state forgets 'inactive' at the two execution boundaries:
--
--   * when execution begins: at the first non-slew execution event, which is
--     where v_generator_params puts not_started -> ongoing and where
--     too_trigger_accept() (V1275) accepts a ToO request.  Only the first:
--     suspending an observation mid-step must survive that step's own later
--     events.
--
--   * when execution ends: when obscalc stores a Completed workflow.  Natural
--     completion depends on the generated sequence, so obscalc is the first
--     place it becomes visible.  (Declared completion is taken from Ongoing,
--     never from Inactive, so it has nothing to clear.)
--
-- Clearing the user state invalidates the observation's obscalc row, so a
-- Completed observation is recalculated once more and then settles.
--
-- 'ready' is left alone in both places.  A ToO's request is accepted at the
-- same first event, so the user state no longer drives its trigger.

-------------------------------------------------------------------------------
-- Execution begins.
-------------------------------------------------------------------------------

CREATE FUNCTION execution_start_clears_inactive()
  RETURNS trigger AS $$
BEGIN
  IF NOT EXISTS (
    SELECT 1
      FROM t_execution_event e
     WHERE e.c_observation_id      = NEW.c_observation_id
       AND e.c_event_type         <> 'slew'::e_execution_event_type
       AND e.c_execution_event_id <> NEW.c_execution_event_id
  ) THEN
    UPDATE t_observation
       SET c_workflow_user_state = NULL
     WHERE c_observation_id      = NEW.c_observation_id
       AND c_workflow_user_state = 'inactive'::e_workflow_user_state;
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER execution_start_clears_inactive_trigger
  AFTER INSERT ON t_execution_event
  FOR EACH ROW
  WHEN (NEW.c_event_type <> 'slew'::e_execution_event_type)
  EXECUTE FUNCTION execution_start_clears_inactive();

-------------------------------------------------------------------------------
-- Execution ends.
-------------------------------------------------------------------------------

CREATE FUNCTION completion_clears_inactive()
  RETURNS trigger AS $$
BEGIN
  UPDATE t_observation
     SET c_workflow_user_state = NULL
   WHERE c_observation_id      = NEW.c_observation_id
     AND c_workflow_user_state = 'inactive'::e_workflow_user_state;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER completion_clears_inactive_trigger
  AFTER INSERT OR UPDATE OF c_workflow_state ON t_obscalc
  FOR EACH ROW
  WHEN (NEW.c_workflow_state = 'completed'::e_workflow_state)
  EXECUTE FUNCTION completion_clears_inactive();

-------------------------------------------------------------------------------
-- Backfill.
-------------------------------------------------------------------------------

-- Until now execution always won, so on every observation that has begun
-- executing (or was declared ongoing or complete) an Inactive user state was
-- invisible in the workflow.  Clearing it changes no workflow state; it only
-- stops the raw-user-state filters from leaving these observations out.  No
-- staff suspension can be among them: that transition is new with this change.
-- The observations' obscalc rows are invalidated, as for any user state edit.
UPDATE t_observation o
   SET c_workflow_user_state = NULL
 WHERE o.c_workflow_user_state = 'inactive'::e_workflow_user_state
   AND (
     o.c_declared_state IS NOT NULL
     OR EXISTS (
       SELECT 1
         FROM t_execution_event e
        WHERE e.c_observation_id = o.c_observation_id
          AND e.c_event_type    <> 'slew'::e_execution_event_type
     )
   );

COMMENT ON FUNCTION execution_start_clears_inactive() IS
  'Clears an inactive user state at an observation''s first non-slew execution '
  'event, so an inactive state stored before execution cannot override it.';

COMMENT ON FUNCTION completion_clears_inactive() IS
  'Clears an inactive user state once obscalc stores a completed workflow; it '
  'has no meaning there and would hide the observation from raw-state filters.';
