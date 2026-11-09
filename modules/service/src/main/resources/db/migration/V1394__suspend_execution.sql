-- Staff may suspend an executing observation: set an Ongoing observation
-- Inactive, and set it back.
--
-- WHERE THE SUSPENSION LIVES
--
-- Not in t_observation.c_workflow_user_state.  That column says what the PI (or
-- staff, before execution) asked for, and execution has always overridden it:
-- an observation set Inactive and then executed anyway is Ongoing.  A
-- suspension must do the opposite, overriding execution, so it can't share the
-- column without something to tell the two apart, and telling them apart would
-- mean rewriting the user state at the boundaries of execution.  Those writes
-- would come from event processing and from obscalc, which have locked
-- t_obscalc by then, while every observation edit locks t_observation first
-- and t_obscalc second: a deadlock.
--
-- So the suspension is a flag of its own, on t_observation_execution.  That is
-- the per-observation row event processing already locks first (V1227), so the
-- order stays
--
--     t_observation_execution  ->  t_obscalc  ->  t_visit
--
-- and t_observation is still neither locked nor written by event processing.
--
-- WHAT IT MEANS
--
-- An Ongoing observation that is suspended is Inactive.  A Completed one stays
-- Completed, and an observation that isn't executing can't be suspended.  The
-- flag is left alone when the observation completes: completion overrides it,
-- and should the observation stop being complete it is still suspended, as
-- staff left it.  Only staff resuming it clears the flag.
--
-- The PI's user state is untouched by a suspension, so a ToO observation's
-- 'ready' survives being suspended and resumed.

ALTER TABLE t_observation_execution
  ADD COLUMN c_suspended boolean NOT NULL DEFAULT false;

COMMENT ON COLUMN t_observation_execution.c_suspended IS
  'Set by staff to suspend an executing observation, which is then Inactive '
  'until resumed (unless complete).';

-- Suspending or resuming changes the workflow, which obscalc stores, and it
-- changes the observation as clients see it.
CREATE FUNCTION observation_suspension_invalidate()
  RETURNS trigger AS $$
BEGIN
  CALL invalidate_obscalc(NEW.c_observation_id);
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER observation_suspension_invalidate_trigger
  AFTER UPDATE OF c_suspended ON t_observation_execution
  FOR EACH ROW
  WHEN (OLD.c_suspended IS DISTINCT FROM NEW.c_suspended)
  EXECUTE FUNCTION observation_suspension_invalidate();

CREATE TRIGGER observation_suspension_edit_trigger
  AFTER UPDATE OF c_suspended ON t_observation_execution
  FOR EACH ROW
  WHEN (OLD.c_suspended IS DISTINCT FROM NEW.c_suspended)
  EXECUTE FUNCTION ch_observation_edit_by_observation();

-- For queries that leave inactive observations out by their user state, and
-- should leave suspended ones out too.  A completed observation isn't
-- suspended, whatever its flag; completion is the workflow state obscalc
-- stored.
CREATE FUNCTION observation_is_suspended(p_observation_id d_observation_id)
  RETURNS boolean
  LANGUAGE sql STABLE AS $$
  SELECT EXISTS (
    SELECT 1
      FROM t_observation_execution x
      LEFT JOIN t_obscalc c ON c.c_observation_id = x.c_observation_id
     WHERE x.c_observation_id = p_observation_id
       AND x.c_suspended
       AND c.c_workflow_state IS DISTINCT FROM 'completed'::e_workflow_state
  );
$$;

COMMENT ON FUNCTION observation_is_suspended(d_observation_id) IS
  'Whether staff have suspended the observation, and it isn''t complete.';
