-- "Supersession" replaces a live trigger request when an observation's ToO
-- activation level changes.  For example, when triggered as a RAPID ToO followed
-- by updating the scheduling mode to INTERRUPTING. These are considered
-- distinct triggers.  The first, RAPID trigger becomes SUPERSEDED and a new
-- INTERRUPTING trigger is inserted.
--
-- This migration updates too_trigger_track_ready() to ensure that a live
-- trigger actually exists before marking it superseded and adding a new request.

CREATE OR REPLACE FUNCTION too_trigger_track_ready()
  RETURNS trigger AS $$
DECLARE
  was_triggered bool := TG_OP = 'UPDATE'
                    AND OLD.c_workflow_user_state IS NOT DISTINCT FROM 'ready'::e_workflow_user_state
                    AND OLD.c_too_activation <> 'none'::e_too_activation
                    AND NOT OLD.c_has_unresolved_too_target;
  is_triggered  bool := NEW.c_workflow_user_state IS NOT DISTINCT FROM 'ready'::e_workflow_user_state
                    AND NEW.c_too_activation <> 'none'::e_too_activation
                    AND NOT NEW.c_has_unresolved_too_target;

  -- Guarded on TG_OP exactly as was_triggered is, so OLD is only read on UPDATE.
  activation_changed bool := TG_OP = 'UPDATE'
                         AND NEW.c_too_activation IS DISTINCT FROM OLD.c_too_activation;

  superseded_id d_too_trigger_id;
BEGIN
  -- A new trigger.
  IF is_triggered AND NOT was_triggered THEN
    INSERT INTO t_too_trigger (c_observation_id, c_program_id, c_too_activation)
    VALUES (NEW.c_observation_id, NEW.c_program_id, NEW.c_too_activation)
    ON CONFLICT DO NOTHING;

  -- A trigger withdrawal
  ELSIF was_triggered AND NOT is_triggered THEN
    UPDATE t_too_trigger
       SET c_status = 'withdrawn'
     WHERE c_observation_id = NEW.c_observation_id
       AND c_status = 'requested';

  -- Update the ToO activation by superseding the existing request.
  ELSIF was_triggered AND is_triggered AND activation_changed THEN
    UPDATE t_too_trigger
       SET c_status = 'superseded'
     WHERE c_observation_id = NEW.c_observation_id
       AND c_status = 'requested'
    RETURNING c_too_trigger_id INTO superseded_id;

    IF superseded_id IS NOT NULL THEN
      INSERT INTO t_too_trigger (c_observation_id, c_program_id, c_too_activation, c_supersedes)
      VALUES (NEW.c_observation_id, NEW.c_program_id, NEW.c_too_activation, superseded_id)
      ON CONFLICT DO NOTHING;
    END IF;
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;
