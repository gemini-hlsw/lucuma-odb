-- When an observation's instrument changes, the previously materialized
-- sequence and any manually-defined atoms become invalid.

-- Make the FK from t_atom back to t_observation deferrable so the trigger
-- can run within the same transaction without a constraint violation.
ALTER TABLE t_atom
  DROP CONSTRAINT t_atom_c_observation_id_c_instrument_fkey,
  ADD  CONSTRAINT t_atom_c_observation_id_c_instrument_fkey
    FOREIGN KEY (c_observation_id, c_instrument)
    REFERENCES t_observation(c_observation_id, c_instrument)
    ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED;


CREATE OR REPLACE FUNCTION clear_sequence_on_instrument_change()
  RETURNS TRIGGER AS $$
BEGIN
  DELETE FROM t_sequence_materialization
   WHERE c_observation_id = OLD.c_observation_id;

  -- Other foreign key constraints will prevent deletion of executed sequences.
  DELETE FROM t_atom
   WHERE c_observation_id = OLD.c_observation_id
     AND c_instrument      = OLD.c_instrument;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER clear_sequence_on_instrument_change_trigger
AFTER UPDATE OF c_instrument ON t_observation
FOR EACH ROW
WHEN (
  OLD.c_instrument IS NOT NULL AND
  OLD.c_instrument IS DISTINCT FROM NEW.c_instrument
)
EXECUTE FUNCTION clear_sequence_on_instrument_change();
