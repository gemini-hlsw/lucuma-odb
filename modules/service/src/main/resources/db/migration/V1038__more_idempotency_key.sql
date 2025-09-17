ALTER TABLE t_atom_record
  ADD COLUMN c_idempotency_key uuid,
  ADD CONSTRAINT t_atom_record_unique_idempotency_key UNIQUE (c_idempotency_key);

ALTER TABLE t_step_record
  ADD COLUMN c_idempotency_key uuid,
  ADD CONSTRAINT t_step_record_unique_idempotency_key UNIQUE (c_idempotency_key);

ALTER TABLE t_visit
  ADD COLUMN c_idempotency_key uuid,
  ADD CONSTRAINT t_visit_unique_idempotency_key UNIQUE (c_idempotency_key);

-- update v_step_record
DROP VIEW v_step_record;

-- Recreate the view to add first/last event time. Modified from V0971.
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
  s.c_idempotency_key,
  s.c_first_event_time,
  s.c_last_event_time,
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