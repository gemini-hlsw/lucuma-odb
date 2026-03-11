-- Add some indices on t_execution_event, since it figures prominently
CREATE INDEX ON t_execution_event (c_atom_id)
WHERE c_atom_id IS NOT NULL;

CREATE INDEX ON t_execution_event (c_step_id)
WHERE c_step_id IS NOT NULL;

CREATE INDEX ON t_execution_event (c_observation_id);

-- A mapping from step stage to execution state.  When a step event arrives it
-- carries a step stage and we use this mapping to figure the corresponding step
-- execution state.  The mapping is currently complete.  Any new entries not
-- present would be assumed 'ongoing'.
CREATE TABLE t_step_stage_execution_state (
  c_step_stage      e_step_stage PRIMARY KEY,
  c_execution_state d_tag        NOT NULL REFERENCES t_step_execution_state(c_tag)
);

INSERT INTO t_step_stage_execution_state (
  c_step_stage,
  c_execution_state
) VALUES
  ('abort',           'aborted'),
  ('continue',        'ongoing'),
  ('end_configure',   'ongoing'),
  ('end_observe',     'ongoing'),
  ('end_step',      'completed'),
  ('pause',           'ongoing'),
  ('start_configure', 'ongoing'),
  ('start_observe',   'ongoing'),
  ('start_step',      'ongoing'),
  ('stop',            'stopped');

-- Create an atom table.  This will store atoms, both executed and unexecuted.
CREATE TABLE t_atom (
  c_atom_id          d_atom_id        PRIMARY KEY,
  c_observation_id   d_observation_id NOT NULL,
  c_sequence_type    e_sequence_type  NOT NULL,
  c_instrument       d_tag            NOT NULL,

  -- Steps will have a FK reference to t_atom (id, inst) so that they too have
  -- a consistent instrument.
  UNIQUE (c_atom_id, c_instrument),

  -- The observation instrument should not be changed without deleting any
  -- associated atoms.  We don't want more than one instrument per observation.
  CONSTRAINT t_atom_c_observation_id_c_instrument_fkey
    FOREIGN KEY (c_observation_id, c_instrument)
    REFERENCES t_observation(c_observation_id, c_instrument) ON DELETE CASCADE,

  -- The atom index is used to sort *unexecuted* atoms.  Once there are executed
  -- (or executing) steps associated with an atom, the step execution order
  -- determines the atom execution order.
  c_atom_index       integer          NOT NULL,

  -- Descriptive text to indicate how the steps are related for human consumption.
  c_description      text

);

-- Generator params computes the execution state and needs to check the
-- steps associated with an observation and sequence.
CREATE INDEX ON t_atom (c_observation_id, c_sequence_type, c_atom_id);

-- Copy the old t_atom_record data over to t_atom.
INSERT INTO t_atom (
  c_atom_id,
  c_observation_id,
  c_sequence_type,
  c_instrument,
  c_atom_index
)
SELECT
  r.c_atom_id,
  r.c_observation_id,
  r.c_sequence_type,
  r.c_instrument,
  0 as c_atom_index -- not relevant once execution starts
FROM t_atom_record r;

-- Add a breakpoint enum.  We can now store breakpoints.
CREATE TYPE e_breakpoint AS enum(
  'enabled',
  'disabled'
);

-- Create a step table to hold steps, both executing and pending. Execution
-- information is relegated to a t_step_execution table, where an entry is added
-- only when a step event (or dataset event) is received.
CREATE TABLE t_step (
  c_step_id          d_step_id        PRIMARY KEY,
  c_atom_id          d_atom_id        NOT NULL,
  c_instrument       d_tag            NOT NULL,
  c_step_type        e_step_type      NOT NULL,

  -- The step index is used to sort *unexecuted* steps inside of an atom.  Once
  -- a step receives its first event, an execution order is set in stone.  In
  -- other words, executed steps are ordered according to when they started
  -- executing. To-be executed steps are ordered according to the step index.
  c_step_index       integer       NOT NULL,

  c_observe_class    e_obs_class   NOT NULL,
  c_time_estimate    interval      NOT NULL DEFAULT '00:00:00'::interval,
  c_offset_p         d_angle_µas   NOT NULL DEFAULT 0,
  c_offset_q         d_angle_µas   NOT NULL DEFAULT 0,
  c_guide_state      e_guide_state NOT NULL DEFAULT 'enabled'::e_guide_state,

  c_breakpoint       e_breakpoint  NOT NULL DEFAULT 'disabled'::e_breakpoint,

  -- For FK references from t_step_config, t_gmos_north_dynamic etc.
  UNIQUE (c_step_id, c_instrument),
  UNIQUE (c_step_id, c_step_type),

  -- We'll reference the atom and instrument, ensuring we don't mix in steps for
  -- different instruments.  We'll cascade deletes, but note that
  -- t_step_execution will prevent deletion of steps with execution information.
  CONSTRAINT t_atom_c_atom_id_c_instrument_fkey
    FOREIGN KEY (c_atom_id, c_instrument)
    REFERENCES t_atom(c_atom_id, c_instrument) ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED

);

CREATE INDEX ON t_step (c_atom_id);

-- We'll keep up with a step execution order counter in t_observation.  Each
-- executed (or executing) step gets a unique order.
ALTER TABLE t_observation
ADD COLUMN c_step_execution_order integer NOT NULL DEFAULT 0;

-- Step execution information, which exists once the first step (or dataset)
-- event associated with the step arrives.
CREATE TABLE t_step_execution (
  c_step_id          d_step_id  PRIMARY KEY REFERENCES t_step(c_step_id),

  -- denormalize these columns for efficiency and constraint checking
  c_observation_id   d_observation_id NOT NULL REFERENCES t_observation(c_observation_id),
  c_sequence_type    e_sequence_type  NOT NULL,

  c_visit_id         d_visit_id NOT NULL REFERENCES t_visit(c_visit_id),
  c_execution_state  d_tag      NOT NULL REFERENCES t_step_execution_state(c_tag) DEFAULT ('not_started'::character varying)::d_tag,

  -- Execution order determined when the first associated event is added.
  c_execution_order  integer   NOT NULL,

  -- Each executed step is uniquely ordered
  UNIQUE (c_observation_id, c_execution_order),

  c_first_event_time timestamp NOT NULL,
  c_last_event_time  timestamp NOT NULL,

  CHECK (c_first_event_time <= c_last_event_time)

);

CREATE INDEX ON t_step_execution (c_observation_id);

-- One ongoing step at a time.
CREATE UNIQUE INDEX one_ongoing_step_per_observation
ON t_step_execution (c_observation_id)
WHERE c_execution_state = 'ongoing';

-- Copy the old t_step_record data over to t_step.
INSERT INTO t_step (
  c_step_id,
  c_atom_id,
  c_instrument,
  c_step_type,
  c_step_index,
  c_observe_class,
  c_time_estimate,
  c_offset_p,
  c_offset_q,
  c_guide_state
)
SELECT
  r.c_step_id,
  r.c_atom_id,
  r.c_instrument,
  r.c_step_type,
  r.c_step_index,
  r.c_observe_class,
  r.c_time_estimate,
  r.c_offset_p,
  r.c_offset_q,
  r.c_guide_state
FROM t_step_record r;

-- Copy the old t_step_record data over to t_step_execution.
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
SELECT
  r.c_step_id,
  a.c_observation_id,
  a.c_sequence_type,
  a.c_visit_id,
  CASE
    WHEN NOT es.c_terminal THEN 'abandoned'
    ELSE r.c_execution_state
  END AS c_execution_state,
  ROW_NUMBER() OVER (
    PARTITION BY a.c_observation_id
    ORDER BY r.c_created, r.c_step_id
  ) AS c_execution_order,
  r.c_first_event_time,
  r.c_last_event_time
FROM t_step_record r
JOIN t_atom_record a           ON a.c_atom_id = r.c_atom_id
JOIN t_step_execution_state es ON es.c_tag = r.c_execution_state
WHERE r.c_first_event_time IS NOT NULL
  AND r.c_last_event_time  IS NOT NULL;

-- Set the new execution order field in t_observation.
UPDATE t_observation o
   SET c_step_execution_order = COALESCE(m.max_order, 0)
  FROM (
    SELECT
      c_observation_id,
      MAX(c_execution_order) AS max_order
    FROM t_step_execution
    GROUP BY c_observation_id
  ) m
 WHERE o.c_observation_id = m.c_observation_id;

-- Ensure that the visit and execution order are never changed.
CREATE OR REPLACE FUNCTION validate_step_execution_update()
RETURNS TRIGGER AS $$
BEGIN

  -- Don't allow the visit to change.
  IF OLD.c_visit_id IS NOT NULL
     AND OLD.c_visit_id <> NEW.c_visit_id THEN
    RAISE EXCEPTION 'c_visit_id is write-once in t_step_execution: cannot change from % to %', OLD.c_visit_id, NEW.c_visit_id
    USING ERRCODE = 'check_violation';
  END IF;

  -- Don't allow the execution order to change.
  IF OLD.c_execution_order IS NOT NULL
     AND OLD.c_execution_order <> NEW.c_execution_order THEN
    RAISE EXCEPTION 'c_execution_order is write-once in t_step_execution: cannot change from % to %', OLD.c_execution_order, NEW.c_execution_order
    USING ERRCODE = 'check_violation';
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER validate_step_execution_update_trigger
BEFORE UPDATE ON t_step_execution
FOR EACH ROW
EXECUTE FUNCTION validate_step_execution_update();

-- Create a view on the atom table.  This will correspond to the old atom record
-- table in that it only contains (at least partially) executed atoms.  There
-- may be multiple atom records (one per visit) for each atom.
CREATE VIEW v_atom_record AS
SELECT
  (a.c_atom_id::text || se.c_visit_id::text) AS c_atom_visit_id,
  a.c_atom_id,
  se.c_visit_id,
  a.c_observation_id,
  a.c_sequence_type,
  a.c_description,

  MIN(se.c_execution_order)      AS c_execution_order,
  MIN(se.c_first_event_time)     AS c_first_event_time,
  MAX(se.c_last_event_time)      AS c_last_event_time,

  CASE
    WHEN bool_and(es.c_terminal) THEN 'completed'
    ELSE 'ongoing'
  END::d_tag AS c_execution_state

FROM t_atom a
JOIN t_step s                  ON s.c_atom_id  = a.c_atom_id
JOIN t_step_execution se       ON se.c_step_id = s.c_step_id
JOIN t_step_execution_state es ON es.c_tag     = se.c_execution_state
GROUP BY
  a.c_atom_id,
  se.c_visit_id,
  a.c_observation_id,
  a.c_sequence_type,
  a.c_description;

-- A trigger to insert / update the step execution information when a new
-- step or dataset event arrives.
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

  PERFORM 1 FROM t_observation WHERE c_observation_id = NEW.c_observation_id FOR UPDATE;

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

CREATE TRIGGER update_execution_information_for_step_event_trigger
  AFTER INSERT ON t_execution_event
  FOR EACH ROW
  WHEN (NEW.c_step_id IS NOT NULL AND NEW.c_atom_id IS NOT NULL)
    EXECUTE PROCEDURE update_execution_information_for_step_event();


-- A trigger to update the step execution state when an abort event arrives.
CREATE OR REPLACE FUNCTION update_execution_information_for_sequence_event()
  RETURNS TRIGGER AS $$
BEGIN

  IF NEW.c_sequence_command = 'abort' THEN

    PERFORM 1 FROM t_observation WHERE c_observation_id = NEW.c_observation_id FOR UPDATE;

    UPDATE t_step_execution se
    SET c_execution_state = 'abandoned'
    WHERE se.c_observation_id = NEW.c_observation_id
      AND se.c_execution_state = 'ongoing';

  END IF;

  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER update_execution_information_for_sequence_event_trigger
  AFTER INSERT ON t_execution_event
  FOR EACH ROW
  WHEN (NEW.c_sequence_command IS NOT NULL)
    EXECUTE PROCEDURE update_execution_information_for_sequence_event();

-- Abandon ongoing steps (in both sequence types) when a new visit is recorded.
-- Ideally they'd be abandoned when the previous visit *ends* but we don't have
-- a great way to detect that.
CREATE OR REPLACE FUNCTION update_execution_information_for_visit()
  RETURNS TRIGGER AS $$
BEGIN

  PERFORM 1 FROM t_observation WHERE c_observation_id = NEW.c_observation_id FOR UPDATE;

  UPDATE t_step_execution se
  SET c_execution_state = 'abandoned'
  WHERE se.c_execution_state = 'ongoing'
    AND se.c_visit_id <> NEW.c_visit_id;

  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER update_execution_information_for_visit_trigger
  AFTER INSERT ON t_visit
  FOR EACH ROW
  EXECUTE PROCEDURE update_execution_information_for_visit();

-- A function to mark ongoing steps as abandoned and remove unexecuted atoms and
-- steps
CREATE OR REPLACE PROCEDURE abandon_ongoing_and_delete_unexecuted_steps(
  p_observation_id d_observation_id,
  p_sequence_type  e_sequence_type
) AS $$
BEGIN

  PERFORM 1 FROM t_observation WHERE c_observation_id = p_observation_id FOR UPDATE;

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

-- Update the step record view
DROP VIEW v_step_record;

-- Create a view on the step table.  This will correspond to the old step record
-- table in that it only contains executed steps.  It pulls in all the step-
-- specific data and aggregates the dataset QA state.
CREATE VIEW v_step_record AS
SELECT
  (s.c_atom_id::text || se.c_visit_id::text) AS c_atom_visit_id,
  se.c_step_id,
  s.c_atom_id,
  se.c_visit_id,
  se.c_execution_state,
  se.c_execution_order,
  se.c_first_event_time,
  se.c_last_event_time,
  s.c_instrument,
  s.c_step_type,
  s.c_observe_class,
  s.c_time_estimate,
  s.c_breakpoint,
  g.c_gcal_continuum,
  g.c_gcal_ar_arc,
  g.c_gcal_cuar_arc,
  g.c_gcal_thar_arc,
  g.c_gcal_xe_arc,
  g.c_gcal_filter,
  g.c_gcal_diffuser,
  g.c_gcal_shutter,
  m.c_smart_gcal_type,
  s.c_offset_p,
  s.c_offset_q,
  s.c_guide_state,
  (SELECT MAX(c_qa_state) FROM t_dataset d WHERE d.c_step_id = se.c_step_id) AS c_qa_state
FROM
  t_step_execution se
JOIN t_step s ON s.c_step_id = se.c_step_id
JOIN t_atom a ON a.c_atom_id = s.c_atom_id
LEFT JOIN t_step_config_gcal g       ON g.c_step_id = se.c_step_id
LEFT JOIN t_step_config_smart_gcal m ON m.c_step_id = se.c_step_id;

-- These are no longer used as we'll delete t_step_record.
DROP TRIGGER update_step_record_event_times_trigger ON t_execution_event;
DROP FUNCTION update_step_record_event_times;

-- Sequence serialization coordination
CREATE TABLE t_sequence_materialization (
  c_observation_id d_observation_id NOT NULL REFERENCES t_observation(c_observation_id),
  c_sequence_type  e_sequence_type  NOT NULL,

  CONSTRAINT t_sequence_materialization_pkey PRIMARY KEY (c_observation_id, c_sequence_type),

  c_created        timestamp        NOT NULL,
  c_updated        timestamp        NOT NULL
);

-- We'll say that every observation with an executed step has been materialized.
INSERT INTO t_sequence_materialization (
  c_observation_id,
  c_sequence_type,
  c_created,
  c_updated
)
SELECT
  se.c_observation_id,
  se.c_sequence_type,
  MIN(se.c_first_event_time) AS c_created,
  MIN(se.c_first_event_time) AS c_updated
FROM t_step_execution se
GROUP BY se.c_observation_id, se.c_sequence_type;

-- When a new step is completed (or uncompleted if that ever happens), we need
-- to poke obscalc.
DROP TRIGGER  step_record_invalidate_trigger ON t_step_record;
DROP FUNCTION step_record_invalidate;

CREATE OR REPLACE FUNCTION step_obscalc_invalidate()
  RETURNS TRIGGER AS $$
DECLARE
  obs_id d_observation_id;
BEGIN
  SELECT a.c_observation_id INTO STRICT obs_id
    FROM t_step s
    JOIN t_atom a ON a.c_atom_id = s.c_atom_id
   WHERE s.c_step_id = OLD.c_step_id;

  CALL invalidate_obscalc(obs_id);

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER step_obscalc_invalidate_trigger
AFTER UPDATE ON t_step_execution FOR EACH ROW WHEN (
  OLD.c_execution_state IS DISTINCT FROM NEW.c_execution_state AND (
    OLD.c_execution_state = 'completed' OR
    NEW.c_execution_state = 'completed'
  )
) EXECUTE FUNCTION step_obscalc_invalidate();

-- Deletes the observation's execution digest when one of its steps is marked
-- complete.
DROP TRIGGER delete_execution_digest_trigger ON t_step_record;
DROP FUNCTION delete_execution_digest;

CREATE OR REPLACE FUNCTION delete_execution_digest()
  RETURNS TRIGGER AS $$
BEGIN

  DELETE FROM t_execution_digest WHERE c_observation_id = NEW.c_observation_id;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Triggers the deletion of an observation's execution digest when a step is
-- marked complete (or marked incomplete having previously been marked complete).
CREATE TRIGGER delete_execution_digest_on_update_trigger
AFTER UPDATE ON t_step_execution FOR EACH ROW
WHEN (
  OLD.c_execution_state IS DISTINCT FROM NEW.c_execution_state AND (
    OLD.c_execution_state = 'completed' OR
    NEW.c_execution_state = 'completed'
  )
) EXECUTE FUNCTION delete_execution_digest();

CREATE TRIGGER delete_execution_digest_on_insert_trigger
AFTER INSERT ON t_step_execution FOR EACH ROW
WHEN (NEW.c_execution_state = 'completed')
EXECUTE FUNCTION delete_execution_digest();

-- Transfer the FK constraints from t_atom_record to t_atom and from
-- t_step_record to t_step.
ALTER TABLE t_dataset
  DROP CONSTRAINT t_dataset_c_step_id_fkey;

ALTER TABLE t_dataset
  ADD CONSTRAINT t_dataset_c_step_id_fkey FOREIGN KEY (c_step_id) REFERENCES t_step(c_step_id);

ALTER TABLE t_execution_event
  DROP CONSTRAINT t_execution_event_c_atom_id_fkey,
  DROP CONSTRAINT t_execution_event_c_step_id_fkey;

ALTER TABLE t_execution_event
  ADD CONSTRAINT t_execution_event_c_atom_id_fkey FOREIGN KEY (c_atom_id) REFERENCES t_atom(c_atom_id),
  ADD CONSTRAINT t_execution_event_c_step_id_fkey FOREIGN KEY (c_step_id) REFERENCES t_step(c_step_id);

ALTER TABLE t_step_config_gcal
  DROP CONSTRAINT t_step_config_gcal_c_step_id_c_step_type_fkey;

ALTER TABLE t_step_config_gcal
  ADD CONSTRAINT t_step_config_gcal_c_step_id_c_step_type_fkey FOREIGN KEY (c_step_id, c_step_type) REFERENCES t_step(c_step_id, c_step_type) ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED;

ALTER TABLE t_step_config_smart_gcal
  DROP CONSTRAINT t_step_config_smart_gcal_c_step_id_c_step_type_fkey;

ALTER TABLE t_step_config_smart_gcal
  ADD CONSTRAINT t_step_config_smart_gcal_c_step_id_c_step_type_fkey FOREIGN KEY (c_step_id, c_step_type) REFERENCES t_step(c_step_id, c_step_type) ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED;

ALTER TABLE t_flamingos_2_dynamic
  DROP CONSTRAINT t_flamingos_2_dynamic_c_step_id_c_instrument_fkey;

ALTER TABLE t_flamingos_2_dynamic
  ADD CONSTRAINT t_flamingos_2_dynamic_c_step_id_c_instrument_fkey FOREIGN KEY (c_step_id, c_instrument) REFERENCES t_step(c_step_id, c_instrument) ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED;

ALTER TABLE t_gmos_north_dynamic
  DROP CONSTRAINT t_gmos_north_dynamic_c_step_id_c_instrument_fkey;

ALTER TABLE t_gmos_north_dynamic
  ADD CONSTRAINT t_gmos_north_dynamic_c_step_id_c_instrument_fkey FOREIGN KEY (c_step_id, c_instrument) REFERENCES t_step(c_step_id, c_instrument) ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED;

ALTER TABLE t_gmos_south_dynamic
  DROP CONSTRAINT t_gmos_south_dynamic_c_step_id_c_instrument_fkey;

ALTER TABLE t_gmos_south_dynamic
  ADD CONSTRAINT t_gmos_south_dynamic_c_step_id_c_instrument_fkey FOREIGN KEY (c_step_id, c_instrument) REFERENCES t_step(c_step_id, c_instrument) ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED;

-- Rework the dataset visit, obs id, index assignment.  Before we knew that the
-- visit existed because datasets referred to steps that referred to atoms that
-- referred to visits and each layer had to exist and be assigned.  Now atoms
-- and steps are written before the visit is applied so the visit could in
-- theory be null at the time a new dataset is added.  We'll reject that.

DROP TRIGGER set_datatset_observation_trigger ON t_dataset;
DROP FUNCTION set_dataset_observation;

DROP TRIGGER set_initial_observation_reference_in_dataset_trigger ON t_dataset;
DROP FUNCTION set_initial_observation_reference_in_dataset;

DROP TRIGGER set_datatset_visit_trigger ON t_dataset;
DROP FUNCTION set_dataset_visit;

DROP TRIGGER check_dataset_visit_immutable_trigger ON t_dataset;
DROP FUNCTION check_dataset_visit_immutable;

CREATE FUNCTION initialize_dataset_before_insert()
RETURNS TRIGGER AS $$
DECLARE
  step_id        d_step_id;
  visit_id       d_visit_id;
  observation_id d_observation_id;
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
  -- reference.  This assumes that the step has an entry in t_step_execution.
  -- If not, we'll raise a check violation and bail.
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

  IF visit_id IS NULL THEN
    RAISE EXCEPTION
      'Cannot insert dataset for unexecuted step %',
      NEW.c_step_id
    USING
      ERRCODE = 'check_violation',
      DETAIL  = 'A row (with execution order) must exist in t_step_execution before datasets may be added.',
      HINT    = 'Ensure the step has execution events before inserting datasets.';
  END IF;

  NEW.c_visit_id              := visit_id;
  NEW.c_observation_id        := observation_id;
  NEW.c_step_index            := exec_order;
  NEW.c_observation_reference := obs_ref;

  RETURN NEW;

END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER initialize_dataset_before_insert_trigger
BEFORE INSERT ON t_dataset
FOR EACH ROW
EXECUTE FUNCTION initialize_dataset_before_insert();