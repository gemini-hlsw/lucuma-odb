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