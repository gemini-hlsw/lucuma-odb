-- Ties ToO triggers into the observation workflow.
--
-- V1242 introduced triggers as a standalone record; nothing consumed them. This
-- migration makes an accepted trigger the thing that puts a ToO observation into
-- the 'ready' state, which has two consequences here: the set of triggers that
-- may be live at once narrows, and a trigger change has to invalidate the cached
-- workflow the way any other observation edit does.
--
-- CORRECTIONS TO V1242
--
-- V1242's comments describe 'accepted' as terminal and 'requested' as the only
-- non-terminal status. Both are now wrong, and that file cannot be edited (it is
-- applied; Flyway checksums it). The COMMENT statements at the bottom of this
-- file are the current description; prefer them over the prose in V1242.
--
-- The lifecycle is now:
--
--   requested --> accepted   --> (stays accepted; the observation executes)
--             |            \
--             |             --> withdrawn   (only before execution begins)
--             +--> denied
--             +--> withdrawn
--
-- 'accepted' is NOT terminal: it may still be withdrawn right up until the
-- observation starts executing, which is the point of no return rather than
-- acceptance itself. 'denied' and 'withdrawn' are terminal and, for the purpose
-- of requesting again, equivalent to never having been triggered.
--
-- Deliberately absent is any trigger status meaning "execution has begun". That
-- fact already lives in v_observation.c_execution_state, computed live from
-- t_execution_event, and mirroring it here would be a second copy to keep in
-- sync for no gain. TooTriggerService.withdraw reads the view directly.

-------------------------------------------------------------------------------
-- At most one LIVE trigger per observation.
-------------------------------------------------------------------------------

-- V1242 scoped this to 'requested' alone, on the assumption that acceptance was
-- terminal. It isn't: an accepted trigger is what holds the observation 'ready',
-- so a second request while one is accepted would mean two triggers claiming the
-- same observation, and withdrawing either would leave the state ambiguous.
--
-- 'denied' and 'withdrawn' stay outside the index, which is what lets a PI try
-- again after a refusal.
--
-- If this DROP/CREATE fails on a live database it is because some observation
-- already holds both a 'requested' and an 'accepted' trigger -- reachable under
-- V1242's index. Failing loudly is intended: the fix is a judgement call about
-- whose trigger survives, not something to paper over here.
DROP INDEX i_too_trigger_active;

CREATE UNIQUE INDEX i_too_trigger_active
  ON t_too_trigger (c_observation_id)
  WHERE (c_status IN ('requested', 'accepted'));

-------------------------------------------------------------------------------
-- Workflow invalidation.
-------------------------------------------------------------------------------

-- The workflow state is computed, not stored, and t_obscalc caches the result.
-- Now that an accepted trigger is an input to that computation, a trigger row
-- changing has to invalidate the cache exactly as an observation edit does --
-- without this, accepting a trigger changes nothing anyone can see.
CREATE TRIGGER too_trigger_obscalc_invalidate_trigger
  AFTER INSERT OR UPDATE OR DELETE ON t_too_trigger
  FOR EACH ROW
  EXECUTE FUNCTION obsid_obscalc_invalidate();

-------------------------------------------------------------------------------
-- Documentation.
-------------------------------------------------------------------------------

COMMENT ON TYPE e_too_trigger_status IS
  'Lifecycle status of a ToO trigger. requested -> accepted is what makes the '
  'observation ready; accepted may still be withdrawn until execution begins. '
  'denied and withdrawn are terminal and permit a fresh request.';

COMMENT ON TABLE t_too_trigger IS
  'One row per attempt to activate a ToO observation. At most one row per '
  'observation may be requested-or-accepted at a time (i_too_trigger_active); '
  'denied and withdrawn attempts accumulate as history.';

COMMENT ON COLUMN t_too_trigger.c_status IS
  'See e_too_trigger_status. An accepted row is what holds the observation in '
  'the ready workflow state.';
