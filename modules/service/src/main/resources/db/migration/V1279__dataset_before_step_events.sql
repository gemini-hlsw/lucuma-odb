-- Lets a dataset be recorded for a step the ODB has not seen an event for, so
-- Observe can send step and dataset traffic in any order. recordDataset used to
-- fail with "has not been started" whenever t_step_execution had no row for the
-- step: the dataset's step index is that row's c_execution_order and nothing else
-- can supply it.
--
-- A dataset now creates the row, ongoing. recordDataset requires service access
-- and is the only path into t_dataset, so it can only mean an instrument is
-- writing a frame for that step right now -- the dataset stands in for the step
-- event it implies. So the step becomes the observation's ongoing step and any
-- other ongoing step is abandoned, exactly as a step event does (V1227), and as
-- one_ongoing_step_per_observation requires.
--
-- An existing row is left untouched, so a late dataset cannot reopen a completed
-- step. The visit comes from the dataset being inserted; recordDataset has
-- always required a visitId and ignored it on this path.
--
-- The mutex is taken in a BEFORE INSERT trigger, so it is held before the
-- insert's foreign keys take FOR KEY SHARE -- the V1217/V1227 lock order -- and
-- across both the abandon and the insert, which is what keeps concurrent datasets
-- off one_ongoing_step_per_observation.

-- One owner for the per-observation step counter, so the dataset path and the
-- step-event path cannot drift. The caller must already hold
-- lock_observation_execution for the observation.
CREATE FUNCTION next_step_execution_order(p_observation_id d_observation_id)
  RETURNS integer AS $$
DECLARE
  next_order integer;
BEGIN
  UPDATE t_observation_execution
     SET c_step_execution_order = c_step_execution_order + 1
   WHERE c_observation_id = p_observation_id
  RETURNING c_step_execution_order INTO next_order;
  RETURN next_order;
END;
$$ LANGUAGE plpgsql;

-- Body identical to V1234 except that the counter bump is now the function above.
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

    new_order := next_step_execution_order(NEW.c_observation_id);

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

-- The dataset path.  Body identical to V1100 except that a missing
-- t_step_execution row is now created instead of rejected.
CREATE OR REPLACE FUNCTION initialize_dataset_before_insert()
RETURNS TRIGGER AS $$
DECLARE
  step_id        d_step_id;
  visit_id       d_visit_id;
  observation_id d_observation_id;
  sequence_type  e_sequence_type;
  exec_order     integer;
  obs_ref        varchar;
BEGIN

  -- Make sure the step even exists.  This will be a FK violation anyway but we
  -- otherwise wouldn't catch it before trying to do the visit lookup below
  -- and we'll report a misleading error.
  SELECT c_step_id INTO step_id FROM t_step WHERE c_step_id = NEW.c_step_id;

  IF step_id IS NULL THEN
    RAISE EXCEPTION
      'Step % not found',
      NEW.c_step_id
    USING
      ERRCODE = 'foreign_key_violation',
      DETAIL  = 'A row must exist in t_step before datasets may be added.',
      HINT    = 'Ensure the step has associated execution events before inserting datasets.';
  END IF;

  -- Lookup the visit, observation id, execution sequence order and observation
  -- reference from the step's execution row, if it has one.
  SELECT
    se.c_visit_id,
    se.c_observation_id,
    se.c_execution_order,
    o.c_observation_reference
  INTO
    visit_id,
    observation_id,
    exec_order,
    obs_ref
  FROM t_step_execution se
  JOIN t_observation o ON o.c_observation_id = se.c_observation_id
  WHERE se.c_step_id = NEW.c_step_id;

  -- No execution row yet: the dataset creates one.
  IF visit_id IS NULL THEN

    IF NEW.c_visit_id IS NULL THEN
      RAISE EXCEPTION
        'Cannot insert dataset for unexecuted step % without a visit',
        NEW.c_step_id
      USING
        ERRCODE = 'check_violation',
        DETAIL  = 'The step has no execution row yet, so the visit cannot be inferred.',
        HINT    = 'Supply the visit the dataset belongs to.';
    END IF;

    SELECT a.c_observation_id, a.c_sequence_type, o.c_observation_reference
      INTO observation_id, sequence_type, obs_ref
      FROM t_step s
      JOIN t_atom a        ON a.c_atom_id        = s.c_atom_id
      JOIN t_observation o ON o.c_observation_id = a.c_observation_id
     WHERE s.c_step_id = NEW.c_step_id;

    PERFORM lock_observation_execution(observation_id);

    -- Re-check under the mutex: a step event, or another dataset, may have
    -- created the row while we waited.
    SELECT se.c_visit_id, se.c_execution_order
      INTO visit_id, exec_order
      FROM t_step_execution se
     WHERE se.c_step_id = NEW.c_step_id;

    IF visit_id IS NULL THEN

      -- This step is exposing, so any other ongoing step is over.
      UPDATE t_step_execution
         SET c_execution_state = 'abandoned'
       WHERE c_observation_id  = observation_id
         AND c_step_id        <> NEW.c_step_id
         AND c_execution_state = 'ongoing';

      exec_order := next_step_execution_order(observation_id);
      visit_id   := NEW.c_visit_id;

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
        observation_id,
        sequence_type,
        visit_id,
        'ongoing', -- a dataset means the step is executing
        exec_order,
        now(),
        now()
      );
    END IF;

  END IF;

  NEW.c_visit_id              := visit_id;
  NEW.c_observation_id        := observation_id;
  NEW.c_step_index            := exec_order;
  NEW.c_observation_reference := obs_ref;

  RETURN NEW;

END;
$$ LANGUAGE plpgsql;
