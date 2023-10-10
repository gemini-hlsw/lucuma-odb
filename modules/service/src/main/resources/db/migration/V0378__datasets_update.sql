-- There are no real datasets yet, but I want to add a new non null column so
-- truncate first to remove possible any test data.
TRUNCATE TABLE t_dataset_event;
TRUNCATE TABLE t_dataset;

-- Add the observation id to t_dataset to enable quick operations on all of an
-- observation's datasets.  Otherwise getting to the observation requires
-- joining with the step record, atom record, and finally observation tables.
ALTER TABLE t_dataset
  ADD c_observation_id d_observation_id NOT NULL REFERENCES t_observation(c_observation_id) ON DELETE CASCADE;

-- Switch to GID dataset ids.
CREATE DOMAIN d_dataset_id AS varchar
  CHECK (VALUE ~ '^d-[1-9a-f][0-9a-f]*$');
COMMENT ON DOMAIN d_dataset_id IS 'Dataset ID';

-- Add a dataset id as the primary key, in lieu of the step/id pair.
CREATE SEQUENCE s_dataset_id START WITH 256; -- three hex digits
ALTER TABLE t_dataset
  DROP CONSTRAINT t_dataset_pkey,
  ADD COLUMN c_dataset_id d_dataset_id PRIMARY KEY DEFAULT 'd-' || to_hex(nextval('s_dataset_id')),
  ADD UNIQUE (c_step_id, c_index);

-- Reference the dataset id.
ALTER TABLE t_dataset_event
  DROP COLUMN c_step_id,
  DROP COLUMN c_index,
  ADD COLUMN c_dataset_id d_dataset_id NOT NULL REFERENCES t_dataset(c_dataset_id);

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