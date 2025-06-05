CREATE TYPE e_workflow_state
  AS ENUM('inactive', 'undefined', 'unapproved', 'defined', 'ready', 'ongoing', 'completed');

ALTER TABLE t_obscalc
  ADD COLUMN c_workflow_state       e_workflow_state   NOT NULL DEFAULT 'undefined',
  ADD COLUMN c_workflow_transitions e_workflow_state[] NOT NULL DEFAULT ARRAY['inactive'::e_workflow_state],
  ADD COLUMN c_workflow_validations jsonb              NOT NULL DEFAULT '[]'::jsonb;

-- Set all existing observations to pending so that the workflow is calculated
UPDATE t_obscalc
  SET c_obscalc_state     = 'pending',
      c_last_invalidation = now();