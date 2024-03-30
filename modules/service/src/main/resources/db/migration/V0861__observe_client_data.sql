
ALTER TABLE t_atom_record
  ADD COLUMN c_generated_id d_atom_id;

COMMENT ON COLUMN t_atom_record.c_generated_id IS 'Generated, original source atom id, if any.';

ALTER TABLE t_step_record
  ADD COLUMN c_generated_id d_step_id;

COMMENT ON COLUMN t_step_record.c_generated_id IS 'Generated, original step id, if any.';

-- Update the view to include the generated step id.
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
  s.c_generated_id,
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