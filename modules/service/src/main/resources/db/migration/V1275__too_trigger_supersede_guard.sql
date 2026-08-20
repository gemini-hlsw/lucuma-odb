-- Supersession replaces a live request; it must not invent one.
--
-- V1267's supersession arm closes the outstanding request out and inserts its
-- successor.  The insert was unconditional, on the reasoning that was_triggered
-- guaranteed a row to supersede.  V1273 breaks that guarantee: acceptance closes
-- the request but leaves the observation's 'ready' state alone, so an accepted
-- observation is still "asking" and is_triggered stays true for good.  An
-- activation change under an executing or finished observation would then find
-- nothing to supersede and insert a brand new 'requested' row -- a live request
-- for work already done, and precisely the thing 'accepted' exists to prevent.
--
-- Guarding the insert on having actually superseded something is the whole fix.
-- The alternative was to clear 'ready' at acceptance, which restores the old
-- guarantee but writes t_observation from inside an execution-event trigger, in
-- the opposite lock order to every other writer of that pair; see V1273.
--
-- The withdrawal and creation arms are unchanged, and so is the trigger.

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

    -- Only replace a request that actually existed.  superseded_id is null when
    -- the observation is still asking but has no live request, which since V1273
    -- is the ordinary state of an observation whose request has been accepted:
    -- 'ready' survives acceptance, so is_triggered stays true for the rest of the
    -- observation's life.  Inserting unconditionally would mint a fresh live
    -- request every time the activation moved under an executing or finished
    -- observation -- a request for work already done.
    IF superseded_id IS NOT NULL THEN
      INSERT INTO t_too_trigger (c_observation_id, c_program_id, c_too_activation, c_supersedes)
      VALUES (NEW.c_observation_id, NEW.c_program_id, NEW.c_too_activation, superseded_id)
      ON CONFLICT DO NOTHING;
    END IF;
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;
