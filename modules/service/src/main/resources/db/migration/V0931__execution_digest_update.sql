CREATE TYPE e_execution_state AS ENUM(
  'not_defined',
  'not_started',
  'ongoing',
  'completed'
);

-- Add the execution state to the sequence digest
--
ALTER TABLE t_execution_digest
  ADD COLUMN c_acq_execution_state e_execution_state NOT NULL DEFAULT 'not_defined',
  ADD COLUMN c_sci_execution_state e_execution_state NOT NULL DEFAULT 'not_defined';