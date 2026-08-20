-- Makes the ceiling sweep stop the observation asking, instead of only closing
-- its request.
--
-- WHAT HAPPENED
--
-- V1268 withdrew requests above a newly lowered ceiling but left
-- c_workflow_user_state at 'ready'.  That is the one place where the trigger
-- status and the observation disagree, and it strands the observation: nothing
-- can revive the request, because too_trigger_track_ready() fires on a
-- *transition* of the user state and the state never moved.  Restore the ceiling
-- and the observation computes back to READY with no live request -- a ready ToO
-- that no observer will ever see.
--
-- WHAT CHANGES
--
-- The sweep now clears 'ready' on the affected observations and lets the existing
-- withdrawal arm of too_trigger_track_ready() set the status, rather than setting
-- it here.  That is strictly less machinery: the withdrawal already means exactly
-- this ("the observation stopped asking"), and the sweep is left with one
-- statement instead of two.
--
-- It also sidesteps a deadlock the obvious fix would have introduced.  V1268
-- writes only t_too_trigger, so it never had an ordering problem; but appending
-- the t_observation write to it would have produced t_too_trigger ->
-- t_observation, which is the order that deadlocks against a concurrent
-- observation edit -- confirmed: that shape deadlocks, this one does not.  Going
-- through the withdrawal arm makes the writes t_observation -> t_too_trigger, the
-- direction every other path takes, with no explicit lock needed.
--
-- This relies on a *live* request implying a ready observation, which still holds:
-- the creation arm only ever mints one for an observation that is asking.  Note
-- the converse does not hold once a request has been accepted -- 'ready' survives
-- acceptance (V1273) -- which is why the sweep matches on c_status rather than on
-- the observation's state.
--
-- RESULT
--
-- A lowered ceiling withdraws the request and returns the observation to DEFINED.
-- Raising the ceiling again does not resurrect it: the TAC revoked the
-- authorization, so the PI asks again, which mints a new attempt through the
-- ordinary path.  The trigger on t_proposal is unchanged; only the body moves.

CREATE OR REPLACE FUNCTION too_trigger_ceiling_withdraw()
  RETURNS trigger AS $$
BEGIN
  UPDATE t_observation o
     SET c_workflow_user_state = NULL
   WHERE o.c_workflow_user_state = 'ready'
     AND EXISTS (
       SELECT 1
         FROM t_too_trigger t
        WHERE t.c_observation_id  = o.c_observation_id
          AND t.c_program_id      = NEW.c_program_id
          AND t.c_status          = 'requested'
          AND t.c_too_activation  > NEW.c_too_activation
     );

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

COMMENT ON FUNCTION too_trigger_ceiling_withdraw() IS
  'Clears the ready state of observations whose outstanding ToO request exceeds a '
  'newly lowered ceiling. The withdrawal arm of too_trigger_track_ready() then '
  'closes the request out, so the status and the observation stay in step and the '
  'writes go t_observation -> t_too_trigger like everything else.';
