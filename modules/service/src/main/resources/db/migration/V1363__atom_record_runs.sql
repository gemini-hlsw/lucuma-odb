-- sc-5176: an atom record is now a run of consecutive steps of one atom within
-- one visit, rather than every step of the atom in the visit. Observe can
-- reinsert an acquisition in the middle of a science atom, after which the
-- science atom resumes. Grouping by atom and visit alone folded the science
-- steps before and after the acquisition into a single record, so walking the
-- atom records in order no longer produced the steps in execution order.

-- A run is a sequence of steps of one atom executed back to back within one
-- visit. Every step in a run stores the run's starting point: the execution
-- order of the run's first step.
ALTER TABLE t_step_execution
  ADD COLUMN c_run_order integer;

-- A step starts a run unless the step executed just before it in the
-- observation belongs to the same atom and visit. The run order is then the
-- execution order of the latest run start.
WITH ordered AS (
  SELECT
    se.c_step_id,
    se.c_observation_id,
    se.c_execution_order,
    s.c_atom_id  IS DISTINCT FROM LAG(s.c_atom_id)  OVER w OR
    se.c_visit_id IS DISTINCT FROM LAG(se.c_visit_id) OVER w AS c_starts_run
  FROM t_step_execution se
  JOIN t_step s ON s.c_step_id = se.c_step_id
  WINDOW w AS (PARTITION BY se.c_observation_id ORDER BY se.c_execution_order)
), runs AS (
  SELECT
    c_step_id,
    MAX(CASE WHEN c_starts_run THEN c_execution_order END) OVER (
      PARTITION BY c_observation_id
      ORDER BY c_execution_order
    ) AS c_run_order
  FROM ordered
)
UPDATE t_step_execution se
   SET c_run_order = r.c_run_order
  FROM runs r
 WHERE se.c_step_id = r.c_step_id;

ALTER TABLE t_step_execution
  ALTER COLUMN c_run_order SET NOT NULL;

-- Like the visit and the execution order, the run order is write-once.
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

  -- Don't allow the run order to change.
  IF OLD.c_run_order IS NOT NULL
     AND OLD.c_run_order <> NEW.c_run_order THEN
    RAISE EXCEPTION 'c_run_order is write-once in t_step_execution: cannot change from % to %', OLD.c_run_order, NEW.c_run_order
    USING ERRCODE = 'check_violation';
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Assigns the run order when the step's first event arrives.
CREATE OR REPLACE FUNCTION update_execution_information_for_step_event()
  RETURNS TRIGGER AS $$
DECLARE
  sequence_type    e_sequence_type;
  step_stage_state d_tag;
  new_order        integer;
  run_order        integer;
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

    -- The step continues the run of the step executed just before it if both
    -- belong to the same atom and visit.  Otherwise it starts a new run.
    SELECT p.c_run_order INTO run_order
      FROM t_step_execution p
      JOIN t_step ps ON ps.c_step_id = p.c_step_id
      JOIN t_step s  ON s.c_step_id  = NEW.c_step_id
     WHERE p.c_observation_id = NEW.c_observation_id
       AND p.c_execution_order = (
         SELECT MAX(c_execution_order)
           FROM t_step_execution
          WHERE c_observation_id = NEW.c_observation_id
            AND c_execution_order < new_order
       )
       AND p.c_visit_id = NEW.c_visit_id
       AND ps.c_atom_id = s.c_atom_id;

    INSERT INTO t_step_execution (
      c_step_id,
      c_observation_id,
      c_sequence_type,
      c_visit_id,
      c_execution_state,
      c_execution_order,
      c_run_order,
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
      COALESCE(run_order, new_order),
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


-- Atom records are keyed by atom and run. The run order is unique within the
-- observation, so the key no longer needs the visit.
DROP VIEW v_atom_record;
DROP VIEW v_step_record;

-- One atom record per run: consecutive executed steps of one atom within one
-- visit. An atom resumed after other steps, in the same visit or a later one,
-- has several atom records.
CREATE VIEW v_atom_record AS
SELECT
  (a.c_atom_id::text || ':' || se.c_run_order::text) AS c_atom_run_id,
  a.c_atom_id,
  se.c_visit_id,
  a.c_observation_id,
  a.c_instrument,
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
  se.c_run_order,
  se.c_visit_id,
  a.c_observation_id,
  a.c_instrument,
  a.c_sequence_type,
  a.c_description;

-- Create a view on the step table.  This will correspond to the old step record
-- table in that it only contains executed steps.  It pulls in all the step-
-- specific data and aggregates the dataset QA state.
CREATE VIEW v_step_record AS
SELECT
  (s.c_atom_id::text || ':' || se.c_run_order::text) AS c_atom_run_id,
  se.c_step_id,
  s.c_atom_id,
  se.c_visit_id,
  se.c_execution_state,
  se.c_execution_order,
  se.c_run_order,
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
