-- Execution start time: the observation's first execution event other than a
-- slew, matching how v_execution_state decides whether execution has started.
-- Backs `ExecutionTimestamps.startTime`, and the group minimum-interval check
-- (the next observation must start no earlier than the previous one's
-- completion plus the minimum interval).

-- Finds an observation's first non-slew event with a single index lookup.  It
-- also serves v_execution_state's "any non-slew event" test.
CREATE INDEX i_execution_event_observation_non_slew
  ON t_execution_event (c_observation_id, c_effective_time)
  WHERE c_event_type <> 'slew'::e_execution_event_type;

-- As in V1353, with c_start_time and c_execution_state appended, so that a
-- query needing the execution state alongside the timestamps reads it here
-- rather than joining v_execution_state a second time.
--
-- The execution state is computed in a LATERAL subquery fenced with OFFSET 0.
-- Were v_execution_state joined directly, the planner would flatten it and
-- copy its CASE (and the subqueries in it) into each place it is used, here
-- both c_completion_time and c_execution_state, evaluating it twice per row.
-- The fence computes it once; being LATERAL, it is still looked up per
-- observation rather than for the whole table.
CREATE OR REPLACE VIEW v_execution_timestamps AS
SELECT
  o.c_observation_id,
  v.c_first_event_time,
  v.c_last_event_time,
  CASE
    WHEN v.c_last_event_time IS NULL THEN NULL
    WHEN s.c_execution_state IN ('completed'::e_execution_state, 'declared_complete'::e_execution_state)
      THEN v.c_last_event_time
  END AS c_completion_time,
  (
    SELECT MIN(e.c_effective_time)
    FROM   t_execution_event e
    WHERE  e.c_observation_id = o.c_observation_id
      AND  e.c_event_type <> 'slew'::e_execution_event_type
  ) AS c_start_time,
  s.c_execution_state
FROM t_observation o
CROSS JOIN LATERAL (
  SELECT x.c_execution_state
  FROM   v_execution_state x
  WHERE  x.c_observation_id = o.c_observation_id
  OFFSET 0
) s
LEFT JOIN LATERAL (
  SELECT
    MIN(t.c_start) AS c_first_event_time,
    MAX(t.c_end)   AS c_last_event_time
  FROM t_visit t
  WHERE t.c_observation_id = o.c_observation_id
) v ON TRUE;
