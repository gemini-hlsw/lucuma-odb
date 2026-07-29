-- Target-of-Opportunity (ToO) triggers.
--
-- A trigger is a first-class, per-attempt record of an astronomer's request to
-- activate a ToO observation, together with the observer's decision on it. Each
-- attempt is its own row (see the partial unique index below); a re-attempt is a
-- NEW row, so the full attributed history is preserved. Lifecycle transitions are
-- audited in t_chron_too_trigger_update and broadcast live on ch_too_trigger_edit.
--
-- Lifecycle status. 'requested' is the only non-terminal state; 'accepted',
-- 'denied' and 'withdrawn' are all terminal. 'accepted' is terminal regardless of
-- whether the observation ultimately executes.
CREATE TYPE e_too_trigger_status AS ENUM (
  'requested',
  'accepted',
  'denied',
  'withdrawn'
);

-- GID for triggers. Mirrors lucuma.odb.data.TooTrigger (tag 'r').
CREATE DOMAIN d_too_trigger_id AS varchar; -- TODO format check
COMMENT ON DOMAIN d_too_trigger_id IS 'GID type for ToO triggers.';

CREATE SEQUENCE s_too_trigger_id START WITH 256; -- three hex digits

CREATE TABLE t_too_trigger (

  c_too_trigger_id    d_too_trigger_id      PRIMARY KEY DEFAULT 'r-' || to_hex(nextval('s_too_trigger_id')),

  -- The observation this attempt is for. program_id is denormalized so the edit
  -- notification and audit rows carry it without a join.
  c_observation_id    d_observation_id      NOT NULL REFERENCES t_observation(c_observation_id) ON DELETE CASCADE,
  c_program_id        d_program_id          NOT NULL REFERENCES t_program(c_program_id),

  c_status            e_too_trigger_status  NOT NULL DEFAULT 'requested',

  -- Free-text reason accompanying a terminal transition (denial/withdrawal). The
  -- value rides with its transition in the corresponding chron row.
  c_resolution_reason text                  NULL CHECK (btrim(c_resolution_reason) <> ''),

  -- Denormalized convenience for the initial request; per-transition who/when is
  -- recorded in t_chron_too_trigger_update.
  c_requested_at      timestamp             NOT NULL DEFAULT now(),
  c_requested_by      d_user_id             NULL REFERENCES t_user(c_user_id) DEFAULT current_setting('lucuma.user', true),

  -- Last-modification time, maintained by the trigger below.  requestedAt is the
  -- creation time; updatedAt tracks any subsequent transition.
  c_updated_at        timestamp             NOT NULL DEFAULT now(),

  -- program_id must actually be the observation's program.
  FOREIGN KEY (c_program_id, c_observation_id) REFERENCES t_observation (c_program_id, c_observation_id)
);

-- Maintain c_updated_at on every modification.
CREATE OR REPLACE FUNCTION too_trigger_set_updated_at()
  RETURNS trigger AS $$
BEGIN
  NEW.c_updated_at := now();
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER too_trigger_set_updated_at_trigger
  BEFORE UPDATE ON t_too_trigger
  FOR EACH ROW
  EXECUTE PROCEDURE too_trigger_set_updated_at();

-- At most one outstanding request per observation. This is what makes "new row
-- per attempt" safe: history accumulates, but only one 'requested' attempt is
-- ever live. Terminal attempts (accepted/denied/withdrawn) do not block a new one.
CREATE UNIQUE INDEX i_too_trigger_active
  ON t_too_trigger (c_observation_id)
  WHERE (c_status = 'requested');

CREATE INDEX i_too_trigger_observation ON t_too_trigger (c_observation_id);


-- LIVE EDIT NOTIFICATIONS ----------------------------------------------------
-- ch_too_trigger_edit payload:  trigger_id, observation_id, program_id, status, TG_OP
-- (status is included so an observer dashboard can filter to 'requested' without
-- a refetch). Scoped to program readers by TooTriggerTopic, same as other edits.
CREATE OR REPLACE FUNCTION ch_too_trigger_edit()
  RETURNS trigger AS $$
BEGIN
  PERFORM pg_notify(
    'ch_too_trigger_edit',
    NEW.c_too_trigger_id || ',' ||
    NEW.c_observation_id || ',' ||
    NEW.c_program_id     || ',' ||
    NEW.c_status::text   || ',' ||
    TG_OP
  );
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER ch_too_trigger_edit_trigger
  AFTER INSERT OR UPDATE ON t_too_trigger
  DEFERRABLE
  FOR EACH ROW
  EXECUTE PROCEDURE ch_too_trigger_edit();


-- AUDIT / TRANSITION HISTORY (Chronicle) -------------------------------------
-- Each lifecycle transition is its own transaction, so each becomes its own chron
-- row; c_new_status ordered by c_chron_id reconstructs the state machine, and a
-- denial/withdrawal reason rides in the same row as its status change.
CREATE TABLE t_chron_too_trigger_update (

  c_chron_id        d_chron_id            NOT NULL PRIMARY KEY DEFAULT nextval('s_chron_id'),
  c_timestamp       timestamptz           NOT NULL DEFAULT current_timestamp,
  c_user            d_user_id             NULL REFERENCES t_user(c_user_id) DEFAULT current_setting('lucuma.user', true),
  c_transaction_id  xid8                  NOT NULL DEFAULT pg_current_xact_id(),

  c_operation       e_tg_op               NOT NULL,
  c_too_trigger_id  d_too_trigger_id      NOT NULL,

  CONSTRAINT t_chron_too_trigger_update_unique UNIQUE (c_transaction_id, c_operation, c_too_trigger_id),

  c_mod_observation_id    bool NOT NULL,
  c_mod_program_id        bool NOT NULL,
  c_mod_status            bool NOT NULL,
  c_mod_resolution_reason bool NOT NULL,

  c_new_observation_id    d_observation_id,
  c_new_program_id        d_program_id,
  c_new_status            e_too_trigger_status,
  c_new_resolution_reason text
);

CREATE OR REPLACE FUNCTION chron_too_trigger_update()
  RETURNS TRIGGER AS $$
DECLARE
  mod_observation_id    bool := NEW.c_observation_id    IS DISTINCT FROM OLD.c_observation_id;
  mod_program_id        bool := NEW.c_program_id        IS DISTINCT FROM OLD.c_program_id;
  mod_status            bool := NEW.c_status            IS DISTINCT FROM OLD.c_status;
  mod_resolution_reason bool := NEW.c_resolution_reason IS DISTINCT FROM OLD.c_resolution_reason;
BEGIN
  INSERT INTO t_chron_too_trigger_update AS chron (
    c_operation,
    c_too_trigger_id,
    c_mod_observation_id,
    c_mod_program_id,
    c_mod_status,
    c_mod_resolution_reason,
    c_new_observation_id,
    c_new_program_id,
    c_new_status,
    c_new_resolution_reason
  ) VALUES (
    TG_OP::e_tg_op,
    coalesce(OLD.c_too_trigger_id, NEW.c_too_trigger_id),
    mod_observation_id,
    mod_program_id,
    mod_status,
    mod_resolution_reason,
    CASE WHEN mod_observation_id    THEN NEW.c_observation_id    END,
    CASE WHEN mod_program_id        THEN NEW.c_program_id        END,
    CASE WHEN mod_status            THEN NEW.c_status            END,
    CASE WHEN mod_resolution_reason THEN NEW.c_resolution_reason END
  ) ON CONFLICT ON CONSTRAINT t_chron_too_trigger_update_unique DO UPDATE SET
    c_mod_observation_id    = chron.c_mod_observation_id    OR mod_observation_id,
    c_mod_program_id        = chron.c_mod_program_id        OR mod_program_id,
    c_mod_status            = chron.c_mod_status            OR mod_status,
    c_mod_resolution_reason = chron.c_mod_resolution_reason OR mod_resolution_reason,
    c_new_observation_id    = CASE WHEN chron.c_mod_observation_id    OR mod_observation_id    THEN NEW.c_observation_id    END,
    c_new_program_id        = CASE WHEN chron.c_mod_program_id        OR mod_program_id        THEN NEW.c_program_id        END,
    c_new_status            = CASE WHEN chron.c_mod_status            OR mod_status            THEN NEW.c_status            END,
    c_new_resolution_reason = CASE WHEN chron.c_mod_resolution_reason OR mod_resolution_reason THEN NEW.c_resolution_reason END;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER chron_too_trigger_insert_trigger
  AFTER INSERT ON t_too_trigger
  FOR EACH ROW
  EXECUTE PROCEDURE chron_too_trigger_update();

CREATE TRIGGER chron_too_trigger_update_trigger
  AFTER UPDATE ON t_too_trigger
  FOR EACH ROW
  WHEN (OLD IS DISTINCT FROM NEW)
  EXECUTE PROCEDURE chron_too_trigger_update();
