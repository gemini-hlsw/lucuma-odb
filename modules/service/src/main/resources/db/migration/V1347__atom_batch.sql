-- Atoms are inserted in batches. Each batch gets the next number for its
-- observation and sequence type, allocated while holding the sequence's
-- t_sequence_materialization row lock. The highest batch is the current
-- sequence; lower batches are executed history. Resetting a customized
-- acquisition copies the current batch.
ALTER TABLE t_atom
  ADD COLUMN c_batch int4 NOT NULL DEFAULT 0;

-- Existing batches cannot be told apart. Atoms with a pending step form the
-- current batch; fully executed atoms are history.
UPDATE t_atom a
   SET c_batch = 1
 WHERE EXISTS (
   SELECT 1
     FROM t_step s
    WHERE s.c_atom_id = a.c_atom_id
      AND NOT EXISTS (
        SELECT 1 FROM t_step_execution se WHERE se.c_step_id = s.c_step_id
      )
 );

ALTER TABLE t_atom
  ALTER COLUMN c_batch DROP DEFAULT;
