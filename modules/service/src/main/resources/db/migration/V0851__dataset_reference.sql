-- Add a c_observation_reference and c_step_index that are copies of the
-- corresponding values from t_observation and t_step_record respectively.
-- These will be used to format the dataset reference.
ALTER TABLE t_dataset
  ADD COLUMN c_observation_reference text,
  ADD COLUMN c_step_index     int4 CHECK (c_step_index     > 0),
  ADD COLUMN c_exposure_index int4 CHECK (c_exposure_index > 0);

-- Copy the old c_index over to the new c_exposure_index.
UPDATE t_dataset
   SET c_exposure_index = c_index;

-- Update the trigger that sets the index to use the c_exposure_index column
-- instead of c_index.
CREATE OR REPLACE FUNCTION set_dataset_index()
RETURNS TRIGGER AS $$
BEGIN
  NEW.c_exposure_index := COALESCE((SELECT MAX(c_exposure_index) FROM t_dataset WHERE c_step_id = NEW.c_step_id), 0) + 1;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Update the (step, index) index.
ALTER TABLE t_dataset
  DROP CONSTRAINT t_dataset_c_step_id_c_index_key,
  ADD CONSTRAINT t_dataset_c_step_id_c_exposure_index_key UNIQUE (c_step_id, c_exposure_index),
  DROP COLUMN c_index;

-- Set the observation reference value for existing observations.
UPDATE t_dataset AS d
   SET c_observation_reference = o.c_observation_reference
  FROM t_observation o
 WHERE d.c_observation_id = o.c_observation_id;

-- Set the step index value for existing observations.
UPDATE t_dataset AS d
   SET c_step_index = s.c_step_index
  FROM t_step_record s
 WHERE d.c_step_id = s.c_step_id;

 -- Guarantee that the indices are not null
 ALTER TABLE t_dataset
   ALTER COLUMN c_exposure_index SET NOT NULL,
   ALTER COLUMN c_step_index     SET NOT NULL;

-- When a new row is inserted into t_dataset, set the observation reference
-- and the step index.
CREATE OR REPLACE FUNCTION set_initial_observation_reference_in_dataset()
RETURNS TRIGGER AS $$
BEGIN
  SELECT c_observation_reference INTO NEW.c_observation_reference
  FROM t_observation
  WHERE c_observation_id = NEW.c_observation_id;

  SELECT c_step_index INTO NEW.c_step_index
  FROM t_step_record
  WHERE c_step_id = NEW.c_step_id;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER set_initial_observation_reference_in_dataset_trigger
BEFORE INSERT ON t_dataset
FOR EACH ROW
EXECUTE FUNCTION set_initial_observation_reference_in_dataset();

-- When an observation reference in t_observation changes, "cascade" the change
-- down to the the datasets.
CREATE OR REPLACE FUNCTION update_observation_reference_in_dataset()
RETURNS TRIGGER AS $$
BEGIN
  IF NEW.c_observation_reference IS DISTINCT FROM OLD.c_observation_reference THEN
    UPDATE t_dataset
       SET c_observation_reference = NEW.c_observation_reference
     WHERE c_observation_id = NEW.c_observation_id;
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER update_observation_reference_in_dataset_trigger
AFTER UPDATE OF c_observation_reference ON t_observation
FOR EACH ROW
EXECUTE FUNCTION update_observation_reference_in_dataset();

-- Now we're ready to generate the c_dataset_reference.
CREATE OR REPLACE FUNCTION format_dataset_reference(observation_reference text, step_index int4, exposure_index int4)
RETURNS text AS $$
BEGIN
    RETURN CASE
        WHEN observation_reference IS NULL THEN NULL
        ELSE CONCAT(observation_reference, '-', LPAD(step_index::text, 4, '0'), '-', LPAD(exposure_index::text, 4, '0'))
    END;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

ALTER TABLE t_dataset
  ADD COLUMN c_dataset_reference text GENERATED ALWAYS AS (format_dataset_reference(c_observation_reference, c_step_index, c_exposure_index)) STORED UNIQUE;


-- Create a view for the dataset reference, including only datasets with
-- a defined reference.  This makes mapping the reference as optional easier.
CREATE VIEW v_dataset_reference AS
  SELECT
    c_dataset_id,
    c_observation_id,
    c_observation_reference,
    c_step_index,
    c_exposure_index,
    c_dataset_reference
  FROM t_dataset
  WHERE c_dataset_reference IS NOT NULL
  ORDER BY c_dataset_id;