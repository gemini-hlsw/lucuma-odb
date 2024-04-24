CREATE TYPE e_atom_stage AS ENUM(
  'end_atom',
  'start_atom'
);

-- Drop the event type constraint in t_execution_event
ALTER TABLE t_execution_event
  DROP CONSTRAINT check_event_type_conditions;

-- Add the atom event type
ALTER TYPE e_execution_event_type ADD VALUE 'atom' BEFORE 'step';

-- Add a column for the atom stage
ALTER TABLE t_execution_event
  ADD COLUMN c_atom_id    d_atom_id    NULL REFERENCES t_atom_record(c_atom_id),
  ADD COLUMN c_atom_stage e_atom_stage NULL;