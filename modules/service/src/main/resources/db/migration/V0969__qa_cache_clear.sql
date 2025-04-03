-- When we transition from Pass or null to either Fail or Usable or vice versa,
-- delete the execution digest so it is recalculated.  A failing dataset means
-- the corresponding step has to be re-executed.
CREATE OR REPLACE FUNCTION clear_execution_digest_on_qa_state_change()
  RETURNS TRIGGER AS $$
BEGIN
  IF (OLD.c_qa_state IS DISTINCT FROM NEW.c_qa_state) AND (
    ((OLD.c_qa_state IS NULL OR OLD.c_qa_state = 'Pass') AND (NEW.c_qa_state IN ('Fail', 'Usable')))
    OR
    ((OLD.c_qa_state IN ('Fail', 'Usable')) AND (NEW.c_qa_state IS NULL OR NEW.c_qa_state = 'Pass'))
  ) THEN
    DELETE FROM t_execution_digest
      WHERE c_observation_id = NEW.c_observation_id;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER dataset_qa_state_update_trigger
  AFTER UPDATE OF c_qa_state ON t_dataset
  FOR EACH ROW
  EXECUTE FUNCTION clear_execution_digest_on_qa_state_change();