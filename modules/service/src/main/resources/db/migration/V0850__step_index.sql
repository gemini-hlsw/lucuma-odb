-- Add a step index (relative to its observation) in the step record
ALTER TABLE t_step_record
  ADD COLUMN c_step_index int4 CHECK (c_step_index > 0);

-- Migrate existing steps to populate the step index.
WITH numbered_steps AS (
  SELECT
    s.c_step_id,
    ROW_NUMBER() OVER (PARTITION BY a.c_observation_id ORDER BY s.c_created) AS c_step_index
  FROM
    t_step_record AS s
  INNER JOIN
    t_atom_record AS a ON s.c_atom_id = a.c_atom_id
)
UPDATE
  t_step_record AS s
SET
  c_step_index = ns.c_step_index
FROM
  numbered_steps AS ns
WHERE
  s.c_step_id = ns.c_step_id;

-- Now make the step index not nullable.
ALTER TABLE t_step_record
  ALTER COLUMN c_step_index SET NOT NULL;

-- Prevent the step index from changing.
CREATE OR REPLACE FUNCTION prevent_step_index_update()
RETURNS TRIGGER AS $$
BEGIN
  IF NEW.c_step_index != OLD.c_step_index THEN
    RAISE EXCEPTION 'The step index (in column c_step_index) cannot be updated once it is set';
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER prevent_step_index_update_trigger
BEFORE UPDATE on t_step_record
FOR EACH ROW
EXECUTE FUNCTION prevent_step_index_update();

-- Update the view to include the step index.
DROP VIEW v_step_record;

CREATE VIEW v_step_record AS
SELECT
  s.c_step_id,
  s.c_step_index,
  s.c_atom_id,
  s.c_instrument,
  s.c_step_type,
  s.c_observe_class,
  s.c_created,
  s.c_time_estimate,
  g.c_gcal_continuum,
  g.c_gcal_ar_arc,
  g.c_gcal_cuar_arc,
  g.c_gcal_thar_arc,
  g.c_gcal_xe_arc,
  g.c_gcal_filter,
  g.c_gcal_diffuser,
  g.c_gcal_shutter,
  n.c_offset_p,
  n.c_offset_q,
  n.c_guide_state,
  m.c_smart_gcal_type
FROM
  t_step_record s
LEFT JOIN t_step_config_gcal g
  ON g.c_step_id = s.c_step_id
LEFT JOIN t_step_config_science n
  ON n.c_step_id = s.c_step_id
LEFT JOIN t_step_config_smart_gcal m
  ON m.c_step_id = s.c_step_id
ORDER BY
  s.c_step_id;

-- Add the step index to the primary key, and bubble this along to t_dataset.
--ALTER TABLE t_dataset
--  DROP CONSTRAINT t_dataset_c_step_id_fkey,
--  ADD COLUMN c_step_index int4;

--ALTER TABLE t_execution_event
--  DROP CONSTRAINT t_execution_event_c_step_id_fkey;

--ALTER TABLE t_step_record
--  DROP CONSTRAINT t_step_pkey,
--  ADD CONSTRAINT t_step_pkey PRIMARY KEY (c_step_id, c_step_index);

--ALTER TABLE t_execution_event
--  ADD CONSTRAINT t_execution_event_c_step_id_fkey PRIMARY KEY (c_step_id);

--UPDATE
--  t_dataset AS d
--SET
--  d.c_step_index = s.c_step_index
--INNER JOIN
--  t_step_record AS s ON s.c_step_id = d.c_step_id;

--ALTER TABLE t_dataset
--  ADD CONSTRAINT t_dataset_c_step_id_fkey FOREIGN KEY (c_step_id, c_step_index) REFERENCES t_dataset(c_step_id, c_step_index);

 -- Finally add a generated column to holl