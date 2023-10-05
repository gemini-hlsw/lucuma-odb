-- There are no real datasets yet, but I want to add a new non null column so
-- truncate first to remove possible any test data.
TRUNCATE TABLE t_dataset;

-- Add the observation id to t_dataset to enable quick operations on all of an observation's datasets.
ALTER TABLE t_dataset
        ADD c_observation_id d_observation_id NOT NULL REFERENCES t_observation(c_observation_id) ON DELETE CASCADE;

-- Set the dataset's observation.
CREATE FUNCTION set_dataset_observation()
RETURNS TRIGGER AS $$
BEGIN
    SELECT o.c_observation_id
    INTO NEW.c_observation_id
    FROM t_step_record s
    JOIN t_atom_record a ON s.c_atom_id        = a.c_atom_id
    JOIN t_observation o ON a.c_observation_id = o.c_observation_id
    WHERE s.c_step_id = NEW.c_step_id;

    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER set_datatset_observation_trigger
BEFORE INSERT ON t_dataset
FOR EACH ROW
EXECUTE FUNCTION set_dataset_observation();

-- Make sure that the observation isn't reassigned.
CREATE FUNCTION check_dataset_observation_immutable()
RETURNS TRIGGER AS $$
BEGIN
  IF NEW.c_observation_id <> OLD.c_observation_id THEN
    RAISE EXCEPTION 'Cannot change observation id once set';
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER check_dataset_observation_immutable_trigger
BEFORE UPDATE ON t_dataset
FOR EACH ROW
EXECUTE FUNCTION check_dataset_observation_immutable();