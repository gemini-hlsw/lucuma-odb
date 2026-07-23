-- Root-cause fix for the deadlock class that produced V1212 and V1217, replacing
-- the "acquire the strong lock first" approach with "don't take a lock that strong
-- in the first place."
--
-- WHY THIS WORKS
--
-- The execution-information triggers need mutual exclusion only among THEMSELVES:
-- one ongoing step per observation, and a serialized c_step_execution_order
-- increment.  They were written with FOR UPDATE, which is the strongest row lock
-- and -- critically -- the ONLY mode that conflicts with FOR KEY SHARE:
--
--   requested \ held | KEY SHARE | SHARE | NO KEY UPDATE | UPDATE
--   KEY SHARE        |    ok     |  ok   |      ok       |  CONFLICT
--   NO KEY UPDATE    |    ok     | CONF  |    CONFLICT   |  CONFLICT
--   UPDATE           | CONFLICT  | CONF  |    CONFLICT   |  CONFLICT
--
-- FOR KEY SHARE is exactly what PostgreSQL takes on t_observation to enforce the
-- foreign key of every child row inserted (t_execution_event, t_visit, t_atom,
-- t_dataset, t_sequence_materialization, ... -- 46 tables reference it).  So
-- FOR UPDATE was the one choice guaranteed to fight the FK machinery, on the most
-- referenced row in the schema.
--
-- FOR NO KEY UPDATE still conflicts with itself, so the triggers serialize
-- exactly as before -- the invariant is preserved.  But it does NOT conflict with
-- FOR KEY SHARE, so the upgrade KEY SHARE -> NO KEY UPDATE can never deadlock:
--
--   A: holds KEY SHARE(obs), wants NO KEY UPDATE -> B holds only KEY SHARE -> GRANTED
--   B: holds KEY SHARE(obs), wants NO KEY UPDATE -> conflicts with A's -> waits -> proceeds
--
-- No cycle, and no ordering discipline required.  This is why the approach scales:
-- V1217 must be repeated for every FK-bearing child table (and for every one added
-- in future), whereas weakening the mode fixes all of them at once.
--
-- SECONDARY BENEFIT
--
-- While a FOR UPDATE was held, EVERY insert of ANY child row for that observation
-- blocked -- a throughput cost nobody was measuring.  Under FOR NO KEY UPDATE
-- those inserts proceed freely; only the trigger bodies serialize.

-- 1. The BEFORE INSERT trigger from V1217 is no longer needed: there is no longer
--    an upgrade to lose a race on.  (Harmless if left in place, but it takes an
--    unnecessary exclusive lock on every event insert, which is the very
--    contention this migration removes.)
DROP TRIGGER IF EXISTS lock_observation_for_execution_event_trigger ON t_execution_event;
DROP FUNCTION IF EXISTS lock_observation_for_execution_event();

-- 2. Step / dataset events.  Body identical to V1100 except the lock mode.
CREATE OR REPLACE FUNCTION update_execution_information_for_step_event()
  RETURNS TRIGGER AS $$
DECLARE
  sequence_type    e_sequence_type;
  step_stage_state d_tag;
  new_order        integer;
BEGIN

  -- What is the sequence type we're working with?
  SELECT c_sequence_type INTO STRICT sequence_type
  FROM t_atom
  WHERE c_atom_id = NEW.c_atom_id;

  -- What is the execution state according to the step stage? (For step events,
  -- otherwise this is NULL.)
  SELECT s.c_execution_state
  INTO step_stage_state
  FROM t_step_stage_execution_state s
  WHERE s.c_step_stage = NEW.c_step_stage;

  -- Serialize with the other execution-information writers.
  --
  -- FOR NO KEY UPDATE rather than FOR UPDATE: the INSERT INTO t_execution_event
  -- that fired this AFTER trigger has already taken a FOR KEY SHARE lock on this
  -- very t_observation row, enforcing t_execution_event.c_observation_id's
  -- foreign key.  FOR UPDATE is the only mode that conflicts with FOR KEY SHARE,
  -- so requesting it here would be upgrading against a lock other concurrent
  -- inserts hold too -- the deadlock this migration removes. NO KEY UPDATE
  -- conflicts with itself, so these writers still serialize.
  PERFORM 1 FROM t_observation WHERE c_observation_id = NEW.c_observation_id FOR NO KEY UPDATE;

  -- Ensure only one ongoing step in this observation.
  UPDATE t_step_execution se
     SET c_execution_state = 'abandoned'
   WHERE c_observation_id = NEW.c_observation_id
     AND c_step_id       <> NEW.c_step_id
     AND c_execution_state = 'ongoing';

  IF NOT EXISTS (
    SELECT 1 FROM t_step_execution WHERE c_step_id = NEW.c_step_id
  ) THEN

    UPDATE t_observation
       SET c_step_execution_order = c_step_execution_order + 1
     WHERE c_observation_id = NEW.c_observation_id
    RETURNING c_step_execution_order INTO new_order;

    INSERT INTO t_step_execution (
      c_step_id,
      c_observation_id,
      c_sequence_type,
      c_visit_id,
      c_execution_state,
      c_execution_order,
      c_first_event_time,
      c_last_event_time
    )
    VALUES (
      NEW.c_step_id,
      NEW.c_observation_id,
      sequence_type,
      NEW.c_visit_id,
      COALESCE(step_stage_state, 'ongoing'), -- born ongoing
      new_order,
      NEW.c_received,
      NEW.c_received
    );

  ELSE

    UPDATE t_step_execution e
    SET
      c_visit_id         = NEW.c_visit_id, -- we include the visit in order to fail (in validate_step_execution_update()) if it has changed
      c_first_event_time = least(e.c_first_event_time,   NEW.c_received),
      c_last_event_time  = greatest(e.c_last_event_time, NEW.c_received),
      c_execution_state  =
        CASE
          -- If cur state (cs) in a terminal execution state, we stay there.
          WHEN cs.c_terminal THEN e.c_execution_state

          -- If new state (ns) is a terminal state, go ahead
          WHEN ns.c_terminal THEN step_stage_state

          -- Otherwise we're executing
          ELSE 'ongoing'
        END
    FROM t_step_execution_state cs
    LEFT JOIN t_step_execution_state ns ON ns.c_tag = step_stage_state
    WHERE e.c_step_id = NEW.c_step_id
      AND cs.c_tag    = e.c_execution_state;

  END IF;

  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

-- 3. Sequence (abort) events.
CREATE OR REPLACE FUNCTION update_execution_information_for_sequence_event()
  RETURNS TRIGGER AS $$
BEGIN

  IF NEW.c_sequence_command = 'abort' THEN

    PERFORM 1 FROM t_observation WHERE c_observation_id = NEW.c_observation_id FOR NO KEY UPDATE;

    UPDATE t_step_execution se
    SET c_execution_state = 'abandoned'
    WHERE se.c_observation_id = NEW.c_observation_id
      AND se.c_execution_state = 'ongoing';

  END IF;

  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

-- 4. Visit recording.
CREATE OR REPLACE FUNCTION update_execution_information_for_visit()
  RETURNS TRIGGER AS $$
BEGIN

  PERFORM 1 FROM t_observation WHERE c_observation_id = NEW.c_observation_id FOR NO KEY UPDATE;

  UPDATE t_step_execution se
  SET c_execution_state = 'abandoned'
  WHERE se.c_execution_state = 'ongoing'
    AND se.c_visit_id <> NEW.c_visit_id;

  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

-- 5. Time-accounting invalidation (V1212).  The lock here exists purely to pin the
--    canonical order t_observation -> t_obscalc -> t_visit; NO KEY UPDATE pins it
--    just as well against the other writers, all of which now take at least that
--    mode.
CREATE OR REPLACE PROCEDURE invalidate_visit_time_accounting(
  visit_id       d_visit_id,
  observation_id d_observation_id
) LANGUAGE plpgsql AS $$
BEGIN
  PERFORM 1 FROM t_observation WHERE c_observation_id = observation_id FOR NO KEY UPDATE;
  CALL invalidate_obscalc(observation_id);
  UPDATE t_visit SET c_ta_invalidation = now() WHERE c_visit_id = visit_id;
END;
$$;

-- 6. Sequence regeneration.
--
--    The argument that NO KEY UPDATE suffices: every path that could interfere
--    already takes at least NO KEY UPDATE on the same observation row, so they
--    still mutually exclude.  A competing sequence regeneration calls this
--    procedure itself; a competing execution event goes through
--    update_execution_information_for_step_event (which creates the
--    t_step_execution rows this procedure's NOT EXISTS checks read).  What is
--    given up is blocking inserts into child tables that take no explicit lock at
--    all (t_dataset, t_atom_digest, ...), none of which this procedure reads.
CREATE OR REPLACE PROCEDURE abandon_ongoing_and_delete_unexecuted_steps(
  p_observation_id d_observation_id,
  p_sequence_type  e_sequence_type
) AS $$
BEGIN

  PERFORM 1 FROM t_observation WHERE c_observation_id = p_observation_id FOR NO KEY UPDATE;

  -- Abandon ongoing steps.
  UPDATE t_step_execution se
     SET c_execution_state = 'abandoned'
   WHERE se.c_execution_state = 'ongoing'
     AND se.c_observation_id = p_observation_id
     AND se.c_sequence_type  = p_sequence_type;

  -- Delete steps that have no execution information (i.e., unexecuted steps)
  DELETE FROM t_step s
  USING t_atom a
  WHERE s.c_atom_id = a.c_atom_id
    AND a.c_observation_id = p_observation_id
    AND a.c_sequence_type  = p_sequence_type
    AND NOT EXISTS (
      SELECT 1 FROM t_step_execution se WHERE se.c_step_id = s.c_step_id
    );

  -- Delete atoms with no (executed) steps -- all unexecuted steps have been deleted
  -- so only executed steps remain.
  DELETE FROM t_atom a
  WHERE a.c_observation_id = p_observation_id
    AND a.c_sequence_type  = p_sequence_type
    AND NOT EXISTS (
      SELECT 1
        FROM t_step s
        JOIN t_step_execution se USING (c_step_id)
       WHERE s.c_atom_id = a.c_atom_id
    );

END;
$$ LANGUAGE plpgsql;
