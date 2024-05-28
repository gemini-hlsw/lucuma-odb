-- Whenever an event for an atom is received, all existing ongoing atoms for the
-- same observation may be considered abandoned.
ALTER TYPE e_atom_execution_state ADD VALUE 'abandoned';

-- Whenever an event for a step is recevied, all existing ongoing steps for the
-- same observation may be considered abandoned.
ALTER TYPE e_step_execution_state ADD VALUE 'abandoned';