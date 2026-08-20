-- Records, on the trigger itself, the ToO activation it was requested at.
--
-- The UI needs trigger events to carry the activation and to be filterable on
-- it, and needs a change of activation on an outstanding request to produce an
-- event of its own.  All three fall out of putting the activation on the row.
--
-- A CHANGE OF ACTIVATION IS A NEW REQUEST
--
-- Triggering a standard ToO, a rapid one and an interrupting one provoke very
-- distinct notification behaviour downstream: who is told, how fast, and what
-- they are expected to drop.  They are effectively different things, so a
-- request's identity does not survive a change in its character.  The
-- outstanding row is superseded and a successor takes its place, with a new id
-- and a new c_requested_at -- which is correct rather than lossy, since the clock
-- that matters is how long *this* request, at *this* activation, has been
-- outstanding.  c_supersedes chains them, so the root of a chain still answers
-- "when did this observation first go live at any activation".

-------------------------------------------------------------------------------
-- Columns.
-------------------------------------------------------------------------------

ALTER TABLE t_too_trigger
  -- Nullable for now; backfilled and tightened below.
  ADD COLUMN c_too_activation e_too_activation NULL,

  -- The request this one replaced, null for a first request.  Points backwards,
  -- so the chain walks both ways, and UNIQUE because a request is superseded at
  -- most once -- the successor is the only row that may point at it.
  ADD COLUMN c_supersedes d_too_trigger_id NULL
    REFERENCES t_too_trigger(c_too_trigger_id),
  ADD CONSTRAINT too_trigger_supersedes_unique UNIQUE (c_supersedes);

-------------------------------------------------------------------------------
-- Backfill.
-------------------------------------------------------------------------------

ALTER TABLE t_too_trigger DISABLE TRIGGER USER;

UPDATE t_too_trigger t
   SET c_too_activation = GREATEST(o.c_too_activation, 'standard'::e_too_activation)
  FROM t_observation o
 WHERE o.c_observation_id = t.c_observation_id;

ALTER TABLE t_too_trigger ENABLE TRIGGER USER;

ALTER TABLE t_too_trigger
  ALTER COLUMN c_too_activation SET NOT NULL,

  -- Every trigger is for a ToO, so this column cannot take 'none'.  The
  -- observation's column must admit it; this one must not.
  ADD CONSTRAINT too_trigger_activation_not_none
    CHECK (c_too_activation <> 'none'::e_too_activation);

-------------------------------------------------------------------------------
-- Chronicle.
-------------------------------------------------------------------------------

ALTER TABLE t_chron_too_trigger_update
  ADD COLUMN c_mod_too_activation bool NOT NULL DEFAULT false,
  ADD COLUMN c_mod_supersedes     bool NOT NULL DEFAULT false,
  ADD COLUMN c_new_too_activation e_too_activation,
  ADD COLUMN c_new_supersedes     d_too_trigger_id;

ALTER TABLE t_chron_too_trigger_update
  ALTER COLUMN c_mod_too_activation DROP DEFAULT,
  ALTER COLUMN c_mod_supersedes     DROP DEFAULT;

-- Body as V1242, with the two new columns threaded through.  Neither ever
-- changes on an existing row, so on an UPDATE both flags are always false; what
-- makes them worth carrying is the INSERT branch, where OLD is null and every
-- `NEW.x IS DISTINCT FROM OLD.x` is true -- so the creation row records the
-- activation and the predecessor link, and the chronicle can reconstruct a whole
-- supersession chain.
CREATE OR REPLACE FUNCTION chron_too_trigger_update()
  RETURNS TRIGGER AS $$
DECLARE
  mod_observation_id    bool := NEW.c_observation_id    IS DISTINCT FROM OLD.c_observation_id;
  mod_program_id        bool := NEW.c_program_id        IS DISTINCT FROM OLD.c_program_id;
  mod_status            bool := NEW.c_status            IS DISTINCT FROM OLD.c_status;
  mod_resolution_reason bool := NEW.c_resolution_reason IS DISTINCT FROM OLD.c_resolution_reason;
  mod_too_activation    bool := NEW.c_too_activation    IS DISTINCT FROM OLD.c_too_activation;
  mod_supersedes        bool := NEW.c_supersedes        IS DISTINCT FROM OLD.c_supersedes;
BEGIN
  INSERT INTO t_chron_too_trigger_update AS chron (
    c_operation,
    c_too_trigger_id,
    c_mod_observation_id,
    c_mod_program_id,
    c_mod_status,
    c_mod_resolution_reason,
    c_mod_too_activation,
    c_mod_supersedes,
    c_new_observation_id,
    c_new_program_id,
    c_new_status,
    c_new_resolution_reason,
    c_new_too_activation,
    c_new_supersedes
  ) VALUES (
    TG_OP::e_tg_op,
    coalesce(OLD.c_too_trigger_id, NEW.c_too_trigger_id),
    mod_observation_id,
    mod_program_id,
    mod_status,
    mod_resolution_reason,
    mod_too_activation,
    mod_supersedes,
    CASE WHEN mod_observation_id    THEN NEW.c_observation_id    END,
    CASE WHEN mod_program_id        THEN NEW.c_program_id        END,
    CASE WHEN mod_status            THEN NEW.c_status            END,
    CASE WHEN mod_resolution_reason THEN NEW.c_resolution_reason END,
    CASE WHEN mod_too_activation    THEN NEW.c_too_activation    END,
    CASE WHEN mod_supersedes        THEN NEW.c_supersedes        END
  ) ON CONFLICT ON CONSTRAINT t_chron_too_trigger_update_unique DO UPDATE SET
    c_mod_observation_id    = chron.c_mod_observation_id    OR mod_observation_id,
    c_mod_program_id        = chron.c_mod_program_id        OR mod_program_id,
    c_mod_status            = chron.c_mod_status            OR mod_status,
    c_mod_resolution_reason = chron.c_mod_resolution_reason OR mod_resolution_reason,
    c_mod_too_activation    = chron.c_mod_too_activation    OR mod_too_activation,
    c_mod_supersedes        = chron.c_mod_supersedes        OR mod_supersedes,
    c_new_observation_id    = CASE WHEN chron.c_mod_observation_id    OR mod_observation_id    THEN NEW.c_observation_id    END,
    c_new_program_id        = CASE WHEN chron.c_mod_program_id        OR mod_program_id        THEN NEW.c_program_id        END,
    c_new_status            = CASE WHEN chron.c_mod_status            OR mod_status            THEN NEW.c_status            END,
    c_new_resolution_reason = CASE WHEN chron.c_mod_resolution_reason OR mod_resolution_reason THEN NEW.c_resolution_reason END,
    c_new_too_activation    = CASE WHEN chron.c_mod_too_activation    OR mod_too_activation    THEN NEW.c_too_activation    END,
    c_new_supersedes        = CASE WHEN chron.c_mod_supersedes        OR mod_supersedes        THEN NEW.c_supersedes        END;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-------------------------------------------------------------------------------
-- Supersession.
-------------------------------------------------------------------------------

-- Body as V1261, plus a third branch and the activation on the insert.
--
-- The two predicates read: the PI has asked for this observation, it is a Target
-- of Opportunity, and its target is resolved.  Note that `c_too_activation <>
-- 'none'` is the test for *being* a ToO -- too_activation() returns 'none'
-- whenever c_has_too_target is false, so an ordinary observation never gets past
-- it whatever its scheduling mode.  The c_has_unresolved_too_target clause is
-- therefore only ever excluding a genuine ToO whose target has no coordinates
-- yet; on its own it would be satisfied by an observation with no ToO target at
-- all.
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

    -- superseded_id is null if no row was updated, which was_triggered says
    -- should not happen.  Not worth an exception: the successor is then simply a
    -- request with no predecessor, and the invariant self-heals.
    INSERT INTO t_too_trigger (c_observation_id, c_program_id, c_too_activation, c_supersedes)
    VALUES (NEW.c_observation_id, NEW.c_program_id, NEW.c_too_activation, superseded_id)
    ON CONFLICT DO NOTHING;
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-------------------------------------------------------------------------------
-- Notification.
-------------------------------------------------------------------------------

-- ch_too_trigger_edit payload:
--   trigger_id, observation_id, program_id, status, too_activation, TG_OP
--
-- Subscription filtering happens in memory over the topic element, not in the
-- database, so anything a subscriber filters on has to be in this payload.
CREATE OR REPLACE FUNCTION ch_too_trigger_edit()
  RETURNS trigger AS $$
BEGIN
  PERFORM pg_notify(
    'ch_too_trigger_edit',
    NEW.c_too_trigger_id       || ',' ||
    NEW.c_observation_id       || ',' ||
    NEW.c_program_id           || ',' ||
    NEW.c_status::text         || ',' ||
    NEW.c_too_activation::text || ',' ||
    TG_OP
  );
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-------------------------------------------------------------------------------
-- Documentation.
-------------------------------------------------------------------------------

COMMENT ON COLUMN t_too_trigger.c_too_activation IS
  'The ToO activation this request was made at, written at creation and never '
  'changed. A change of activation supersedes the request rather than amending '
  'it. Never ''none'': every trigger is for a Target of Opportunity.';

COMMENT ON COLUMN t_too_trigger.c_supersedes IS
  'The request this one replaced, null for a first request. The root of the chain '
  'is when the observation first went live at any activation.';

COMMENT ON TABLE t_too_trigger IS
  'One row per attempt to activate a ToO observation, maintained by '
  'too_trigger_track_ready() from the observation user state and its derived '
  'activation. At most one row per observation is requested at a time '
  '(i_too_trigger_active); declined, withdrawn and superseded attempts accumulate '
  'as history.';