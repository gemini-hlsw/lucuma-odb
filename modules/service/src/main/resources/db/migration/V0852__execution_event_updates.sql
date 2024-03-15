
-- Reset the step stage enum to pick up abort, continue, pause, stop.
ALTER TYPE e_step_stage ADD VALUE 'abort'    BEFORE 'end_configure';
ALTER TYPE e_step_stage ADD VALUE 'continue' BEFORE 'end_configure';
ALTER TYPE e_step_stage ADD VALUE 'pause'    BEFORE 'start_configure';
ALTER TYPE e_step_stage ADD VALUE 'stop';

-- Add an enum for the slew events.
CREATE TYPE e_slew_stage AS ENUM(
  'start_slew',
  'end_slew'
);

-- Drop the event type constraint in t_execution_event
ALTER TABLE t_execution_event
  DROP CONSTRAINT check_event_type_conditions;

-- Add the slew event type.
ALTER TYPE e_execution_event_type ADD VALUE 'slew' BEFORE 'step';

-- Add a column for the slew stage
ALTER TABLE t_execution_event
  ADD COLUMN c_slew_stage e_slew_stage NULL;

-- Update the event type constraint
ALTER TABLE t_execution_event
  ADD CONSTRAINT check_event_type_conditions CHECK (
    CASE
      WHEN c_event_type = 'sequence' THEN c_sequence_command IS NOT NULL AND c_step_ID IS NULL     AND c_dataset_id IS NULL
      WHEN c_event_type = 'slew'     THEN c_slew_stage       IS NOT NULL AND c_step_id IS NULL     AND c_dataset_id IS NULL
      WHEN c_event_type = 'step'     THEN c_step_stage       IS NOT NULL AND c_step_id IS NOT NULL AND c_dataset_id IS NULL
      WHEN c_event_type = 'dataset'  THEN c_dataset_stage    IS NOT NULL AND c_step_id IS NOT NULL AND c_dataset_id IS NOT NULL
      ELSE FALSE
    END
  );