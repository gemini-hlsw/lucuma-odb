-- Atoms are inserted in batches, one batch per transaction, and now() is the
-- transaction start time, so all atoms in a batch share the same c_created.
-- The batch with the latest c_created is the current sequence; older batches
-- are executed history. Resetting a customized acquisition copies the current
-- batch.
ALTER TABLE t_atom
  ADD COLUMN c_created timestamp NOT NULL DEFAULT now();

-- Existing atoms all get the same default. Push fully executed atoms back to
-- the materialization time so that only atoms with a pending step count as the
-- current batch.
UPDATE t_atom a
   SET c_created = m.c_created
  FROM t_sequence_materialization m
 WHERE m.c_observation_id = a.c_observation_id
   AND m.c_sequence_type  = a.c_sequence_type
   AND NOT EXISTS (
     SELECT 1
       FROM t_step s
      WHERE s.c_atom_id = a.c_atom_id
        AND NOT EXISTS (
          SELECT 1 FROM t_step_execution se WHERE se.c_step_id = s.c_step_id
        )
   );
