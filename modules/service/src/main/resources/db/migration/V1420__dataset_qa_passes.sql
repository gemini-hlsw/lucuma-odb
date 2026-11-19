-- Whether a dataset QA state passes.  A dataset that hasn't been assessed
-- (NULL) counts as passing, but any state other than 'pass' does not,
-- including states that may be added later.  This matches `isPassing` and
-- `orPassing` in lucuma.odb.sequence.syntax.qastate, which the sequence
-- generator uses.
--
-- Everything in the database that asks whether a dataset failed QA goes
-- through this function so that the triggers that react to QA changes and the
-- queries that act on QA failures cannot disagree.
CREATE FUNCTION dataset_qa_passes(qa e_dataset_qa_state)
  RETURNS boolean
  LANGUAGE sql
  IMMUTABLE
  PARALLEL SAFE
AS $$
  SELECT qa IS NULL OR qa = 'pass'
$$;

-- From V1381.  Clears the execution digest when a dataset starts or stops
-- passing QA.
CREATE OR REPLACE FUNCTION clear_execution_digest_on_qa_state_change()
  RETURNS TRIGGER AS $$
BEGIN
  IF dataset_qa_passes(OLD.c_qa_state) <> dataset_qa_passes(NEW.c_qa_state) THEN
    DELETE FROM t_execution_digest
      WHERE c_observation_id = NEW.c_observation_id;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- From V1381.  Invalidates the visit's time accounting when a dataset starts
-- or stops passing QA.
CREATE OR REPLACE FUNCTION dataset_qa_state_invalidate()
  RETURNS TRIGGER AS $$
BEGIN
  IF dataset_qa_passes(OLD.c_qa_state) <> dataset_qa_passes(NEW.c_qa_state) THEN
    CALL invalidate_visit_time_accounting(NEW.c_visit_id, NEW.c_observation_id);
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;
