-- Lowercase the dataset QA state labels to match the lucuma-core tags, in
-- line with the other enums.  Renaming a label is a catalog-only change:
-- columns, constraints and views pick it up automatically, but PL/pgSQL
-- functions keep their source text and must be redefined.

ALTER TYPE e_dataset_qa_state RENAME VALUE 'Pass'   TO 'pass';
ALTER TYPE e_dataset_qa_state RENAME VALUE 'Usable' TO 'usable';
ALTER TYPE e_dataset_qa_state RENAME VALUE 'Fail'   TO 'fail';

-- From V0969.
CREATE OR REPLACE FUNCTION clear_execution_digest_on_qa_state_change()
  RETURNS TRIGGER AS $$
BEGIN
  IF (OLD.c_qa_state IS DISTINCT FROM NEW.c_qa_state) AND (
    ((OLD.c_qa_state IS NULL OR OLD.c_qa_state = 'pass') AND (NEW.c_qa_state IN ('fail', 'usable')))
    OR
    ((OLD.c_qa_state IN ('fail', 'usable')) AND (NEW.c_qa_state IS NULL OR NEW.c_qa_state = 'pass'))
  ) THEN
    DELETE FROM t_execution_digest
      WHERE c_observation_id = NEW.c_observation_id;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- From V1210.
CREATE OR REPLACE FUNCTION dataset_qa_state_invalidate()
  RETURNS TRIGGER AS $$
BEGIN
  IF (OLD.c_qa_state IS DISTINCT FROM NEW.c_qa_state) AND (
    ((OLD.c_qa_state IS NULL OR OLD.c_qa_state = 'pass') AND (NEW.c_qa_state IN ('fail', 'usable')))
    OR
    ((OLD.c_qa_state IN ('fail', 'usable')) AND (NEW.c_qa_state IS NULL OR NEW.c_qa_state = 'pass'))
  ) THEN
    CALL invalidate_visit_time_accounting(NEW.c_visit_id, NEW.c_observation_id);
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;
