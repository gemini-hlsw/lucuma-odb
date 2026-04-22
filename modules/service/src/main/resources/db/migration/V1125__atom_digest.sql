
-- Add the step index and count columns.
TRUNCATE TABLE t_atom_digest;

ALTER TABLE t_atom_digest
  ADD COLUMN c_step_index int4 NOT NULL CHECK (c_step_index >= 0),
  ADD COLUMN c_step_count int4 NOT NULL CHECK (c_step_count >= 1);

-- Update the obscalc rows to mark them all pending, in order to recalculate
-- the atom digests.
UPDATE t_obscalc
  SET c_obscalc_state = 'pending';

