-- Whenever an atom is started all others for the same observation may be
-- considered abandoned.
ALTER TYPE e_atom_execution_state ADD VALUE 'abandoned';