-- Client-supplied event times.
--
-- Until now an execution event's time was always the moment the ODB ingested it.
-- Any delay between the event actually occurring at the telescope (in the Observe
-- application) and it reaching us skews time accounting.  We now let the client
-- optionally send the real event time.  The columns mirror the GraphQL API:
--
--   c_recorded_time  -- when the ODB recorded it, always now() (was c_received);
--                       maps to the `recordedTime` field.
--   c_client_time    -- the raw client-supplied time, NULL when not provided.
--   c_effective_time -- the time we associate with the event: the client time
--                       when supplied, else the recorded time.  Maps to the
--                       `effectiveTime` field.  A generated column so every reader
--                       (triggers included) sees one clean value, and so it can be
--                       indexed.
--
-- Existing rows have no client time, so c_effective_time = c_recorded_time.

ALTER TABLE t_execution_event
  RENAME COLUMN c_received TO c_recorded_time;

ALTER TABLE t_execution_event
  ADD COLUMN c_client_time    timestamp NULL,
  ADD COLUMN c_effective_time timestamp NOT NULL
             GENERATED ALWAYS AS (COALESCE(c_client_time, c_recorded_time)) STORED;

-- Time accounting streams a visit's events `WHERE c_visit_id = ... ORDER BY
-- c_effective_time`, and update_visit_times aggregates MIN/MAX(c_effective_time)
-- per visit on every insert.  Move the (c_visit_id, ...) index onto the effective
-- time and drop the recorded-time index V1229 added: every hot reader now keys on
-- the effective time, nothing filters or orders events by the recorded time within
-- a visit, and removing it saves an index update on the event-insert hot path.
-- (The column rename above left the old index's auto-generated name unchanged.)
CREATE INDEX ON t_execution_event (c_visit_id, c_effective_time);
DROP INDEX t_execution_event_c_visit_id_c_received_idx;

-- Reject an implausible client time rather than let it corrupt accounting.  A
-- client clock can report anything (1970 if unset, a far-future value if ahead);
-- such a value would poison t_visit.c_start/c_end (raw MIN/MAX feeding the
-- cross-visit overlap discount) and the displayed raw execution time.  Bound the
-- value to the visit's own lifetime.
--
-- Both edges are a 5 minute clock-skew tolerance.  A client never reports an
-- event before its visit was created (the visit is always created first, by the
-- slew or recordVisit that opens it), so the low edge only needs to absorb the
-- same skew as the high edge -- the visit's own creation time is itself bounded
-- to within 5 minutes of the wall clock (see the t_visit client-time check).
--
-- Only c_client_time is validated; when it is NULL the event keeps today's
-- behavior and can never be rejected, so nothing changes for clients that do not
-- opt in.  Being a BEFORE INSERT trigger, it runs before the row is written --
-- and so before the insert's foreign-key locks and the heavier AFTER-trigger
-- work -- so an out-of-range time is rejected cheaply.
CREATE FUNCTION check_execution_event_client_time()
  RETURNS TRIGGER AS $$
DECLARE
  visit_created timestamp;
BEGIN
  IF NEW.c_client_time IS NOT NULL THEN
    SELECT c_created INTO visit_created
      FROM t_visit
     WHERE c_visit_id = NEW.c_visit_id;

    IF NEW.c_client_time < visit_created - interval '5 minutes'
       OR NEW.c_client_time > now() + interval '5 minutes' THEN
      -- Custom SQLSTATE (class 'OD' is not used by PostgreSQL) so the service can
      -- match this specific error rather than any raised exception.  Kept in sync
      -- with ClientTimeError.OutOfRangeSqlState in the Scala code.
      RAISE EXCEPTION 'execution event time % is out of range for visit %',
        NEW.c_client_time, NEW.c_visit_id
        USING ERRCODE = 'ODB01';
    END IF;
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER check_execution_event_client_time_trigger
  BEFORE INSERT ON t_execution_event
  FOR EACH ROW EXECUTE FUNCTION check_execution_event_client_time();

-- Repoint the two live denormalizers onto the effective time.

-- Visit start/end (feeds the overlap discount).
CREATE OR REPLACE FUNCTION update_visit_times()
RETURNS TRIGGER AS $$
DECLARE
  visit_id d_visit_id := COALESCE(NEW.c_visit_id, OLD.c_visit_id);
  visit_min timestamp;
  visit_max timestamp;
BEGIN
  SELECT
    min(c_effective_time),
    max(c_effective_time)
  INTO visit_min, visit_max
  FROM t_execution_event e
  WHERE c_visit_id = visit_id;

  UPDATE t_visit
  SET c_start = visit_min,
      c_end   = visit_max
  WHERE c_visit_id = visit_id;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Step execution first/last event times (feeds sequence generation).  Only the
-- time columns move to c_effective_time; the execution-state machine stays keyed
-- to event arrival order and is left exactly as it was.
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

  -- Serialize with the other execution-information writers.  Note this locks
  -- t_observation_execution, NOT t_observation: the INSERT INTO t_execution_event
  -- that fired this trigger already holds FOR KEY SHARE on the observation row,
  -- and taking a conflicting mode on it is the deadlock this migration removes.
  PERFORM lock_observation_execution(NEW.c_observation_id);

  -- Ensure only one ongoing step in this observation.
  UPDATE t_step_execution se
     SET c_execution_state = 'abandoned'
   WHERE c_observation_id = NEW.c_observation_id
     AND c_step_id       <> NEW.c_step_id
     AND c_execution_state = 'ongoing';

  IF NOT EXISTS (
    SELECT 1 FROM t_step_execution WHERE c_step_id = NEW.c_step_id
  ) THEN

    UPDATE t_observation_execution
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
      NEW.c_effective_time,
      NEW.c_effective_time
    );

  ELSE

    UPDATE t_step_execution e
    SET
      c_visit_id         = NEW.c_visit_id, -- we include the visit in order to fail (in validate_step_execution_update()) if it has changed
      c_first_event_time = least(e.c_first_event_time,   NEW.c_effective_time),
      c_last_event_time  = greatest(e.c_last_event_time, NEW.c_effective_time),
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
