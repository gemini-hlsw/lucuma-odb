-- We are removing the idea that atoms are abandoned.  Atoms are complete when
-- no more steps will be executed regardless of which steps may have once been
-- prescribed for that atom.

UPDATE t_atom_record
   SET c_execution_state = 'completed'
 WHERE c_execution_state = 'abandoned';