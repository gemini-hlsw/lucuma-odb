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


-- Update the observation v to include the new column.
-- Re-create this view to include the new index column.
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
    o.c_observation_id,
    o.c_program_id,
    o.c_observation_index,
    p.c_program_reference,
    CONCAT(p.c_program_reference::text, '-', LPAD(o.c_observation_index::text, 4, '0')) AS c_observation_reference
  FROM t_observation o
  JOIN t_program p ON o.c_program_id = p.c_program_id
  WHERE p.c_program_reference IS NOT NULL
  ORDER BY c_observation_id;