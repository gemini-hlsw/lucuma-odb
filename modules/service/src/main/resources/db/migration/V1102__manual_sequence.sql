-- Trigger function to send an edit event when the sequence materialization
-- entry for an observation is inserted or updated
CREATE OR REPLACE FUNCTION ch_observation_edit_sequence_replace()
  RETURNS trigger AS $$
DECLARE
  program_id d_program_id;
BEGIN
  program_id := (SELECT c_program_id FROM t_observation WHERE c_observation_id = NEW.c_observation_id);
  IF program_id IS NOT NULL THEN
    PERFORM pg_notify('ch_observation_edit', NEW.c_observation_id || ',' || program_id  || ',' || 'UPDATE');
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER ch_observation_edit_sequence_replace_trigger
  AFTER INSERT OR UPDATE ON t_sequence_materialization
  FOR EACH ROW
  EXECUTE FUNCTION ch_observation_edit_sequence_replace();

-- Reset obscalc for sequence materialization
CREATE TRIGGER sequence_materialization_invalidate_trigger
  AFTER INSERT OR UPDATE OR DELETE ON t_sequence_materialization
  FOR EACH ROW EXECUTE FUNCTION obsid_obscalc_invalidate();