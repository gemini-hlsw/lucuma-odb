-- Withdraws outstanding ToO trigger requests that a newly lowered ceiling no
-- longer authorizes.
--
-- The proposal's ToO activation ceiling is what the TAC granted.  Until now it
-- was enforced only on the observation's side: a request already outstanding when
-- the ceiling moved beneath it simply stayed outstanding, so an observer could be
-- looking at a live request for a disruption the program is no longer permitted
-- to cause.  The observation does go 'unapproved' and cannot execute, but that is
-- computed asynchronously by obscalc and does nothing to the trigger row.
--
CREATE FUNCTION too_trigger_ceiling_withdraw()
  RETURNS trigger AS $$
BEGIN
  UPDATE t_too_trigger
     SET c_status = 'withdrawn'
   WHERE c_program_id     = NEW.c_program_id
     AND c_status         = 'requested'
     AND c_too_activation > NEW.c_too_activation;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- UPDATE only.  A proposal is created before its program has observations worth
-- triggering, and the two writers that matter -- the TAC editing the ceiling and
-- the freeze at acceptance -- are both updates.
CREATE TRIGGER too_trigger_ceiling_withdraw_trigger
  AFTER UPDATE OF c_too_activation ON t_proposal
  FOR EACH ROW
  WHEN (NEW.c_too_activation IS NOT NULL
        AND NEW.c_too_activation IS DISTINCT FROM OLD.c_too_activation)
  EXECUTE FUNCTION too_trigger_ceiling_withdraw();

COMMENT ON FUNCTION too_trigger_ceiling_withdraw() IS
  'Withdraws outstanding ToO trigger requests above a newly lowered explicit '
  'ceiling. Compares against the activation each request was made at, which for '
  'a live request is the observation''s current activation.';