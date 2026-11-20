-- The executed SCIENCE-type steps of an observation's science sequence, split
-- by outcome.  Together with the remaining count (c_sci_science_count) they
-- give an executing observation's progress: executed / (executed + remaining).
--
-- A step counts once it reaches a terminal execution state.  It is successful
-- when it completed or was stopped and every one of its datasets passes QA or
-- has not yet been assessed (dataset_qa_passes).  It is unsuccessful otherwise:
-- it was aborted or abandoned, or one of its datasets is marked usable or fail.
-- Only the science sequence counts, not the acquisition, and only steps of type
-- science, not calibrations such as flats and arcs.
CREATE FUNCTION executed_science_steps(oid d_observation_id)
  RETURNS TABLE (c_successful int4, c_unsuccessful int4)
  LANGUAGE sql
  STABLE
AS $$
  SELECT
    (COUNT(*) FILTER (WHERE successful))::int4,
    (COUNT(*) FILTER (WHERE NOT successful))::int4
  FROM (
    SELECT
      se.c_execution_state IN ('completed', 'stopped') AND NOT EXISTS (
        SELECT 1
          FROM t_dataset d
         WHERE d.c_step_id = se.c_step_id
           AND NOT dataset_qa_passes(d.c_qa_state)
      ) AS successful
      FROM t_step_execution se
      JOIN t_step s                  ON s.c_step_id = se.c_step_id
      JOIN t_step_execution_state es ON es.c_tag    = se.c_execution_state
     WHERE se.c_observation_id = oid
       AND se.c_sequence_type  = 'science'
       AND s.c_step_type       = 'science'
       AND es.c_terminal
  ) executed
$$;

-- Stored by obscalc alongside the digest, so that the executed and remaining
-- counts come from the same calculation.
ALTER TABLE t_obscalc
  ADD COLUMN c_sci_executed_successful_count   int4 NOT NULL DEFAULT 0 CHECK (c_sci_executed_successful_count   >= 0),
  ADD COLUMN c_sci_executed_unsuccessful_count int4 NOT NULL DEFAULT 0 CHECK (c_sci_executed_unsuccessful_count >= 0);

-- Fill in the observations that have executed steps.  The counts are derived
-- entirely from the database, so there is no need to recalculate them.
UPDATE t_obscalc c
   SET (c_sci_executed_successful_count, c_sci_executed_unsuccessful_count) = (
         SELECT e.c_successful, e.c_unsuccessful
           FROM executed_science_steps(c.c_observation_id) e
       )
 WHERE EXISTS (
         SELECT 1
           FROM t_step_execution se
          WHERE se.c_observation_id = c.c_observation_id
       );
