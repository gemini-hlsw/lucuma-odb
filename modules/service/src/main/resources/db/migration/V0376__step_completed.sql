
-- Add a completion time that is set when the step is complete, but otherwise null
ALTER TABLE t_step_record
  ADD COLUMN c_completed timestamp NULL;

DROP VIEW v_step_record;

-- A view that ties together all the step config tables, primarily to simplify
-- mapping logic.
CREATE VIEW v_step_record AS
SELECT
  s.c_step_id,
  s.c_atom_id,
  s.c_instrument,
  s.c_step_type,
  s.c_created,
  s.c_completed,
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