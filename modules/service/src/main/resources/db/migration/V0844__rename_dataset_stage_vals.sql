-- Rename e_dataset_stage 'start_observe' and 'end_observe' to avoid confusion
-- with 'e_step_stage' 'start_observe' and 'end_observe'.

ALTER TABLE t_execution_event
  ALTER COLUMN c_dataset_stage TYPE text
  USING c_dataset_stage::text;

UPDATE t_execution_event
  SET c_dataset_stage =
    CASE
      WHEN c_dataset_stage = 'start_observe' THEN 'start_expose'
      WHEN c_dataset_stage = 'end_observe'   THEN 'end_expose'
      ELSE c_dataset_stage
    END;

DROP TYPE e_dataset_stage;

CREATE TYPE e_dataset_stage AS ENUM(
  'end_expose',
  'end_readout',
  'end_write',
  'start_expose',
  'start_readout',
  'start_write'
);

ALTER TABLE t_execution_event
  ALTER COLUMN c_dataset_stage TYPE e_dataset_stage
  USING c_dataset_stage::e_dataset_stage;
