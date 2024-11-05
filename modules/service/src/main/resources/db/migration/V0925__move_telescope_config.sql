ALTER TABLE t_step_record
  ADD COLUMN c_offset_p    d_angle_µas   NOT NULL DEFAULT (0),
  ADD COLUMN c_offset_q    d_angle_µas   NOT NULL DEFAULT (0),
  ADD COLUMN c_guide_state e_guide_state NOT NULL DEFAULT ('enabled');

DROP VIEW v_step_record;

CREATE VIEW v_step_record AS
SELECT
  s.c_step_id,
  s.c_atom_id,
  a.c_visit_id,
  s.c_step_index,
  s.c_instrument,
  s.c_step_type,
  s.c_observe_class,
  s.c_created,
  s.c_execution_state,
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
  m.c_smart_gcal_type,
  s.c_offset_p,
  s.c_offset_q,
  s.c_guide_state,
  (SELECT MAX(c_qa_state) FROM t_dataset d WHERE d.c_step_id = s.c_step_id) AS c_qa_state
FROM
  t_step_record s
LEFT JOIN t_step_config_gcal g
  ON g.c_step_id = s.c_step_id
LEFT JOIN t_step_config_smart_gcal m
  ON m.c_step_id = s.c_step_id
LEFT JOIN t_atom_record a
  ON a.c_atom_id = s.c_atom_id
ORDER BY
  s.c_step_id;

-- Copy the existing offset / guide state from t_step_config_science before we
-- delete the table.
UPDATE t_step_record AS r
   SET c_offset_p    = s.c_offset_p,
       c_offset_q    = s.c_offset_q,
       c_guide_state = s.c_guide_state
  FROM t_step_config_science s
 WHERE r.c_step_id = s.c_step_id;

DROP TABLE t_step_config_science;