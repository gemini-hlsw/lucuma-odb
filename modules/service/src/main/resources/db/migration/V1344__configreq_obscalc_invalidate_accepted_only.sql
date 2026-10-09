-- Configuration requests only affect workflows once the proposal is accepted
-- (see ObservationValidator and ObservationWorkflowService), so changes before
-- then invalidate nothing.  In particular, submitting a proposal (which inserts
-- its configuration requests) and withdrawing it (which deletes them) no longer
-- recalculate every observation in the program.
--
-- Acceptance itself recalculates every observation: it assigns the program
-- reference, which is copied to each observation's row, and that update
-- invalidates the observation.  Leaving Accepted clears the reference in the
-- same way.
CREATE OR REPLACE FUNCTION configreq_obscalc_invalidate()
  RETURNS TRIGGER AS $$
DECLARE
  configreq record;
BEGIN
  IF ROW(NEW.*) IS DISTINCT FROM ROW(OLD.*) THEN
    configreq := COALESCE(NEW, OLD);
    IF EXISTS (
      SELECT 1 FROM t_program
      WHERE c_program_id      = configreq.c_program_id
        AND c_proposal_status = 'accepted'
    ) THEN
      CALL invalidate_all_obscalc_for_program(configreq.c_program_id);
    END IF;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;
