
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