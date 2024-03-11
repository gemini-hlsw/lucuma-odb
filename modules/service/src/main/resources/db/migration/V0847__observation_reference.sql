-- Add an index column to the observation table.
ALTER TABLE t_observation
  ADD COLUMN c_observation_index int4 CHECK (c_observation_index > 0),
  ADD CONSTRAINT unique_observation_index_per_program UNIQUE (c_program_id, c_observation_index);

-- A function to obtain the next observation number for a particular program.
CREATE OR REPLACE FUNCTION next_observation_index(pid d_program_id)
RETURNS INT
AS $$
DECLARE
    sequence_name VARCHAR;
BEGIN
    sequence_name := CONCAT('s_', REPLACE(pid::text, '-', '')); -- a hyphen (as in p-123) will not work

    BEGIN
        EXECUTE 'CREATE SEQUENCE IF NOT EXISTS ' || sequence_name;
    EXCEPTION WHEN unique_violation THEN
        NULL; -- sequence exists, this was an attempt to create it in parallel
    END;

    RETURN nextval(sequence_name)::INT;
END;
$$ LANGUAGE plpgsql;

-- Update all existing observations so that they have an index defined.
UPDATE t_observation
  SET c_observation_index = next_observation_index(c_program_id);

-- Make the index required.
ALTER TABLE t_observation
  ALTER COLUMN c_observation_index SET NOT NULL;

-- Add a trigger that sets the observation index when a new observation is
-- created.
CREATE OR REPLACE FUNCTION set_observation_index()
RETURNS TRIGGER AS $$
BEGIN
  NEW.c_observation_index = next_observation_index(NEW.c_program_id);
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER set_observation_index_trigger
BEFORE INSERT ON t_observation
FOR EACH ROW
EXECUTE FUNCTION set_observation_index();

-- Prevent the observation index from changing.
CREATE OR REPLACE FUNCTION prevent_observation_index_update()
RETURNS TRIGGER AS $$
BEGIN
  IF NEW.c_observation_index != OLD.c_observation_index THEN
    RAISE EXCEPTION 'The observation index (in column c_observation_index) cannot be updated once it is set';
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER prevent_observation_index_update_trigger
BEFORE UPDATE on t_observation
FOR EACH ROW
EXECUTE FUNCTION prevent_observation_index_update();

-- Add a c_program_reference that is a copy of the corresponding reference
-- from t_program.  We'll be maintaining it with triggers.  :-/
ALTER TABLE t_observation
  ADD COLUMN c_program_reference text;

-- Set the program reference value for existing observations.
UPDATE t_observation AS o
   SET c_program_reference = p.c_program_reference
  FROM t_program p
 WHERE p.c_program_id = o.c_program_id;

-- When a new row is inserted into t_observation, set the program reference.
CREATE OR REPLACE FUNCTION set_initial_program_reference_in_observation()
RETURNS TRIGGER AS $$
BEGIN
  SELECT c_program_reference INTO NEW.c_program_reference
  FROM t_program
  WHERE c_program_id = NEW.c_program_id;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER set_initial_program_reference_in_observation_trigger
BEFORE INSERT ON t_observation
FOR EACH ROW
EXECUTE FUNCTION set_initial_program_reference_in_observation();

-- When a program reference in t_program changes, "cascade" the change
-- down to the the observations.
CREATE OR REPLACE FUNCTION update_program_reference_in_observation()
RETURNS TRIGGER AS $$
BEGIN
  IF NEW.c_program_reference <> OLD.c_program_reference THEN
    UPDATE t_observation
    SET c_program_reference = NEW.c_program_reference
    WHERE c_program_id = NEW.c_program_id;
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER update_program_reference_in_observation_trigger
AFTER UPDATE OF c_program_reference ON t_program
FOR EACH ROW
EXECUTE FUNCTION update_program_reference_in_observation();

-- Now we're ready to generate the c_observation_reference.
CREATE OR REPLACE FUNCTION format_observation_reference(program_reference text, index int4)
RETURNS text AS $$
BEGIN
    RETURN CASE
        WHEN program_reference IS NULL THEN NULL
        ELSE CONCAT(program_reference, '-', LPAD(index::text, 4, '0'))
    END;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

ALTER TABLE t_observation
  ADD COLUMN c_observation_reference text GENERATED ALWAYS AS (format_observation_reference(c_program_reference, c_observation_index)) STORED UNIQUE;

-- Index on the observation reference.
CREATE INDEX i_observation_reference ON t_observation (c_observation_reference);

-- Re-create v_observation to include the new columns: c_observation_index,
-- c_program_reference, c_observation_reference
DROP VIEW v_observation;
CREATE VIEW v_observation AS
  SELECT o.*,
  CASE WHEN c_explicit_ra              IS NOT NULL THEN c_observation_id END AS c_explicit_base_id,
  CASE WHEN c_air_mass_min             IS NOT NULL THEN c_observation_id END AS c_air_mass_id,
  CASE WHEN c_hour_angle_min           IS NOT NULL THEN c_observation_id END AS c_hour_angle_id,
  CASE WHEN c_observing_mode_type      IS NOT NULL THEN c_observation_id END AS c_observing_mode_id,
  CASE WHEN c_spec_wavelength          IS NOT NULL THEN c_observation_id END AS c_spec_wavelength_id,
  CASE WHEN c_spec_signal_to_noise_at  IS NOT NULL THEN c_observation_id END AS c_spec_signal_to_noise_at_id,
  CASE WHEN c_spec_wavelength_coverage IS NOT NULL THEN c_observation_id END AS c_spec_wavelength_coverage_id,
  CASE WHEN c_spec_focal_plane_angle   IS NOT NULL THEN c_observation_id END AS c_spec_focal_plane_angle_id
  FROM t_observation o;

-- Create a view for the observation reference, including only observations with
-- a defined reference.  This makes mapping the reference as optional easier.
CREATE VIEW v_observation_reference AS
  SELECT
    c_observation_id,
    c_program_id,
    c_observation_index,
    c_program_reference,
    c_observation_reference
  FROM t_observation
 WHERE c_program_reference IS NOT NULL
  ORDER BY c_observation_id;