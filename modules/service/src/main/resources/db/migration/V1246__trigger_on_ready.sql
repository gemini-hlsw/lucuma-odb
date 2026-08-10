-- Ties ToO triggers into the observation workflow, by deriving the trigger from
-- the observation's workflow state rather than the other way around.
--
-- Setting a ToO observation to 'ready' IS the trigger request: the PI has one
-- lever, the state they already use for every other observation, and the trigger
-- row is the record of it.  Clearing 'ready' withdraws the request.  There is no
-- separate request mutation to keep in step with the state, and no way for the
-- two to disagree, because the database maintains the trigger row itself.
--
-- WHY THERE IS NO 'accepted'
--
-- V1242 imagined a per-trigger staff approval.  That duplicates an authorization
-- that has already happened: V1245 freezes the proposal's ToO activation ceiling
-- at acceptance, which is TAC saying in advance how much disruption this program
-- may cause.  Requiring a second approval per trigger would add nothing but
-- latency, on exactly the observations where latency is the point.
--
-- What the observer does keep is a way to say no.  'declined' records that a
-- trigger was seen and passed over, with a reason, so the PI can tell "nobody
-- has looked at this" from "looked at, not doing it".  Declining also clears the
-- observation's user state, so the observation drops back to 'defined' and the
-- state remains the single source of truth.
--
-- The lifecycle is therefore:
--
--   requested --> withdrawn   (PI clears 'ready', or the observation stops being a ToO)
--             +-> declined    (observer says no, with a reason)
--
-- and a requested trigger simply stays requested while the observation executes.
-- Nothing here records "execution has begun": that lives in the execution events,
-- and the workflow already forbids leaving 'ongoing' for 'defined', so a trigger
-- cannot be withdrawn out from under a running observation.

-------------------------------------------------------------------------------
-- Status vocabulary.
-------------------------------------------------------------------------------

-- Postgres cannot drop a value from an enum, so replace the type outright. This
-- is safe precisely because nothing consumed triggers before this migration.
-- 'accepted' folds back to 'requested' (it meant "approved and still waiting")
-- and 'denied' becomes 'declined'.
DROP INDEX i_too_trigger_active;

CREATE TYPE e_too_trigger_status_new AS ENUM (
  'requested',
  'declined',
  'withdrawn'
);

ALTER TABLE t_too_trigger
  ALTER COLUMN c_status DROP DEFAULT,
  ALTER COLUMN c_status TYPE e_too_trigger_status_new
    USING (
      CASE c_status::text
        WHEN 'accepted' THEN 'requested'
        WHEN 'denied'   THEN 'declined'
        ELSE c_status::text
      END
    )::e_too_trigger_status_new;

ALTER TABLE t_chron_too_trigger_update
  ALTER COLUMN c_new_status TYPE e_too_trigger_status_new
    USING (
      CASE c_new_status::text
        WHEN 'accepted' THEN 'requested'
        WHEN 'denied'   THEN 'declined'
        ELSE c_new_status::text
      END
    )::e_too_trigger_status_new;

DROP TYPE e_too_trigger_status;
ALTER TYPE e_too_trigger_status_new RENAME TO e_too_trigger_status;

ALTER TABLE t_too_trigger
  ALTER COLUMN c_status SET DEFAULT 'requested';

-- Recreated as it was in V1242: at most one live request per observation.
-- 'declined' and 'withdrawn' stay outside the index, so a PI who sets 'ready'
-- again after either one gets a fresh trigger.
CREATE UNIQUE INDEX i_too_trigger_active
  ON t_too_trigger (c_observation_id)
  WHERE (c_status = 'requested');

-------------------------------------------------------------------------------
-- The trigger, derived from the observation's user state.
-------------------------------------------------------------------------------

-- A trigger exists exactly while a ToO observation carries the 'ready' user
-- state.  Both halves of that are watched: dropping 'ready' withdraws, and so
-- does lowering the activation to 'none', which stops the observation being a
-- Target of Opportunity at all.
--
-- Declining does not go through here.  It sets 'declined' first and clears the
-- user state second, so the withdrawal arm finds no 'requested' row and the
-- observer's reason survives.
CREATE FUNCTION too_trigger_track_ready()
  RETURNS trigger AS $$
DECLARE
  was_triggered bool := TG_OP = 'UPDATE'
                    AND OLD.c_workflow_user_state IS NOT DISTINCT FROM 'ready'::e_workflow_user_state
                    AND OLD.c_too_activation <> 'none'::e_too_activation;
  is_triggered  bool := NEW.c_workflow_user_state IS NOT DISTINCT FROM 'ready'::e_workflow_user_state
                    AND NEW.c_too_activation <> 'none'::e_too_activation;
BEGIN
  IF is_triggered AND NOT was_triggered THEN
    INSERT INTO t_too_trigger (c_observation_id, c_program_id)
    VALUES (NEW.c_observation_id, NEW.c_program_id)
    ON CONFLICT DO NOTHING;

  ELSIF was_triggered AND NOT is_triggered THEN
    UPDATE t_too_trigger
       SET c_status = 'withdrawn'
     WHERE c_observation_id = NEW.c_observation_id
       AND c_status = 'requested';
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER too_trigger_track_ready_trigger
  AFTER INSERT OR UPDATE OF c_workflow_user_state, c_too_activation ON t_observation
  FOR EACH ROW
  EXECUTE FUNCTION too_trigger_track_ready();

-------------------------------------------------------------------------------
-- Documentation.
-------------------------------------------------------------------------------

COMMENT ON TYPE e_too_trigger_status IS
  'Lifecycle status of a ToO trigger. A trigger is requested by setting the '
  'observation ready and withdrawn by clearing that; declined records that an '
  'observer saw it and chose not to observe.';

COMMENT ON TABLE t_too_trigger IS
  'One row per attempt to activate a ToO observation, maintained by '
  'too_trigger_track_ready() from the observation user state. At most one row '
  'per observation is requested at a time (i_too_trigger_active); declined and '
  'withdrawn attempts accumulate as history.';
