
-- Add the atom id to step and dataset events.
UPDATE t_execution_event AS e
   SET c_atom_id = s.c_atom_id
  FROM t_step_record AS s
 WHERE e.c_step_id = s.c_step_id AND e.c_atom_id IS NULL;

-- Update the event type constraint
ALTER TABLE t_execution_event
  ADD CONSTRAINT check_event_type_conditions CHECK (
    CASE
      WHEN c_event_type = 'sequence' THEN c_sequence_command IS NOT NULL AND c_atom_id IS NULL     AND c_step_ID IS NULL     AND c_dataset_id IS NULL
      WHEN c_event_type = 'slew'     THEN c_slew_stage       IS NOT NULL AND c_atom_id IS NULL     AND c_step_id IS NULL     AND c_dataset_id IS NULL
      WHEN c_event_type = 'atom'     THEN c_atom_stage       IS NOT NULL AND c_atom_id IS NOT NULL AND c_step_id IS NULL     AND c_dataset_id IS NULL
      WHEN c_event_type = 'step'     THEN c_step_stage       IS NOT NULL AND c_atom_id IS NOT NULL AND c_step_id IS NOT NULL AND c_dataset_id IS NULL
      WHEN c_event_type = 'dataset'  THEN c_dataset_stage    IS NOT NULL AND c_atom_id IS NOT NULL AND c_step_id IS NOT NULL AND c_dataset_id IS NOT NULL
      ELSE FALSE
    END
  );

ALTER TABLE t_atom_record
  ADD COLUMN c_execution_state e_atom_execution_state NOT NULL DEFAULT 'not_started';

ALTER TABLE t_step_record
  ADD COLUMN c_execution_state e_step_execution_state NOT NULL DEFAULT 'not_started';

-- Set completed any step records with an existing completion time.
UPDATE t_step_record
   SET c_execution_state = 'completed'
 WHERE c_completed IS NOT NULL;

-- Recreate the view that ties together all the step config tables with the new
-- c_execution_state column.
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
  s.c_execution_state,
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