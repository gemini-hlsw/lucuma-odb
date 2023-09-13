
-- Add a completion time that is set when the step is complete, but otherwise null
ALTER TABLE t_step_record
  ADD COLUMN c_completed timestamp NULL;

DROP VIEW v_step_record;

-- A view that ties together all the step config tables, primarily to simplify
-- mapping logic.
CREATE VIEW v_step_record AS
SELECT
  s.c_step_id,
  s.c_atom_id,
  s.c_instrument,
  s.c_step_type,
  s.c_created,
  s.c_completed,
  g.c_gcal_continuum,
  g.c_gcal_ar_arc,
  g.c_gcal_cuar_arc,
  g.c_gcal_thar_arc,
  g.c_gcal_xe_arc,
  g.c_gcal_filter,
  g.c_gcal_diffuser,
  g.c_gcal_shutter,
  n.c_offset_p,
  n.c_offset_q,
  n.c_guide_state,
  m.c_smart_gcal_type
FROM
  t_step_record s
LEFT JOIN t_step_config_gcal g
  ON g.c_step_id = s.c_step_id
LEFT JOIN t_step_config_science n
  ON n.c_step_id = s.c_step_id
LEFT JOIN t_step_config_smart_gcal m
  ON m.c_step_id = s.c_step_id
ORDER BY
  s.c_step_id;

-- Deletes the observation's execution digest when one of its step is marked
-- complete.
CREATE OR REPLACE FUNCTION delete_execution_digest()
  RETURNS TRIGGER AS $$
BEGIN
   DELETE FROM t_execution_digest
     WHERE (c_program_id, c_observation_id) IN (
       SELECT DISTINCT
         o.c_program_id, o.c_observation_id
       FROM t_observation o
         JOIN t_atom_record a ON a.c_observation_id = o.c_observation_id
         WHERE a.c_atom_id = OLD.c_atom_id
     );
   RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Triggers the deletion of an observation's execution digest when a step is
-- marked complete (or marked incomplete having previously been marked complete).
CREATE TRIGGER delete_execution_digest_trigger
  AFTER UPDATE ON t_step_record
  FOR EACH ROW
  WHEN (num_nulls(NEW.c_completed, OLD.c_completed) = 1)
    EXECUTE PROCEDURE delete_execution_digest();
