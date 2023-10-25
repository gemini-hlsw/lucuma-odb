-- We'll add a not-nullable visit ID column.  Since there are no valid datasets
-- yet, it's easiest to just truncate the table first.
TRUNCATE TABLE t_dataset CASCADE;

-- Add the visit id.
ALTER TABLE t_dataset
  ADD c_visit_id d_visit_id NOT NULL REFERENCES t_visit(c_visit_id) ON DELETE CASCADE;

-- Set the dataset's visit on insert.
CREATE FUNCTION set_dataset_visit()
RETURNS TRIGGER AS $$
BEGIN
  SELECT a.c_visit_id
  INTO NEW.c_visit_id
  FROM t_step_record s
  JOIN t_atom_record a ON s.c_atom_id = a.c_atom_id
  WHERE s.c_step_id = NEW.c_step_id;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER set_datatset_visit_trigger
BEFORE INSERT ON t_dataset
FOR EACH ROW
EXECUTE FUNCTION set_dataset_visit();

-- Make sure that the visit isn't reassigned.
CREATE FUNCTION check_dataset_visit_immutable()
RETURNS TRIGGER AS $$
BEGIN
  IF NEW.c_visit_id <> OLD.c_visit_id THEN
    RAISE EXCEPTION 'Cannot change visit id once set';
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER check_dataset_visit_immutable_trigger
BEFORE UPDATE ON t_dataset
FOR EACH ROW
EXECUTE FUNCTION check_dataset_visit_immutable();