-- Staff may suspend an executing observation: take an Ongoing observation to
-- the new Suspended workflow state, and back.
--
-- WHERE THE SUSPENSION LIVES
--
-- Not in t_observation.c_workflow_user_state.  That column says what the PI (or
-- staff, before execution) asked for, and execution has always overridden it:
-- an observation set Inactive and then executed anyway is Ongoing.  A
-- suspension must do the opposite, overriding execution.  Kept in the same
-- column, an Inactive set before execution would have to be told apart from a
-- suspension by rewriting the user state at the boundaries of execution.  Those
-- writes would come from event processing and from obscalc, which have locked
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
-- An Ongoing observation that is suspended is Suspended.  A Completed one stays
-- Completed, and an observation that isn't executing can't be suspended.  The
-- flag is left alone when the observation completes: completion overrides it,
-- and should the observation stop being complete it is still suspended, as
-- staff left it.  Only staff resuming it clears the flag.
--
-- The PI's user state is untouched by a suspension, so a ToO observation's
-- 'ready' survives being suspended and resumed.
--
-- A per-observation calibration (a telluric, a daytime pinhole or flat) follows
-- its science observation into the suspension, whether or not it has started.

-- Not used in this migration, which is one transaction: a new enum value can't
-- be used before it is committed.
ALTER TYPE e_workflow_state ADD VALUE 'suspended' AFTER 'ongoing';

ALTER TABLE t_observation_execution
  ADD COLUMN c_suspended boolean NOT NULL DEFAULT false;

COMMENT ON COLUMN t_observation_execution.c_suspended IS
  'Set by staff to suspend an executing observation, which is then Suspended '
  'until resumed (unless complete).';

-- Suspending or resuming changes the workflow, which obscalc stores, and it
-- changes the observation as clients see it.
--
-- It changes the workflow of the observation's per-observation calibrations as
-- well (tellurics, daytime pinholes and flats), which follow it into the
-- suspension.  Nothing else would queue them: the calibration service leaves a
-- suspended observation's calibrations alone.  They are found as
-- ObservationValidationInfo.Statements.AssociatedScienceObservation associates
-- them, by group and by the roles in ObsExtract.PerObservationCalibrationRoles
-- (keep the two in step), and invalidated in the same call as the observation so that
-- the rows are locked in id order, as every bulk invalidation locks them.
CREATE FUNCTION observation_suspension_invalidate()
  RETURNS trigger AS $$
DECLARE
  oids d_observation_id[];
BEGIN
  SELECT array_agg(c.c_observation_id) INTO oids
    FROM t_observation s
    JOIN t_observation c ON c.c_group_id = s.c_group_id
   WHERE s.c_observation_id   = NEW.c_observation_id
     AND s.c_calibration_role IS NULL
     AND c.c_calibration_role IN (
           'telluric'::e_calibration_role,
           'daytime_pinhole'::e_calibration_role,
           'daytime_flat'::e_calibration_role
         )
     AND c.c_existence = 'present';

  CALL invalidate_obscalc_many(array_append(oids, NEW.c_observation_id));
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

-- Obscalc stores each workflow's transitions, and GraphQL reads those.  Every
-- Ongoing observation calculated before now stored [Completed] alone, so staff
-- would not be offered the suspension until something else invalidated it.
-- No other state's transitions changed: nothing can be suspended yet.
DO $$
DECLARE
  ongoing d_observation_id[];
BEGIN
  SELECT array_agg(c_observation_id) INTO ongoing
    FROM t_obscalc
   WHERE c_workflow_state = 'ongoing'::e_workflow_state;

  CALL invalidate_obscalc_many(ongoing);
END;
$$;
