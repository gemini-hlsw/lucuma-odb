-- Invalidate the obscalc results for all observations in the program
CREATE OR REPLACE PROCEDURE invalidate_all_obscalc_for_program(
  pid d_program_id
)
  LANGUAGE plpgsql AS $$
DECLARE
  obs_id d_observation_id;
BEGIN
  FOR obs_id IN
    SELECT c_observation_id
    FROM t_observation
    WHERE c_program_id = pid
  LOOP
    CALL invalidate_obscalc(obs_id);
  END LOOP;
END;
$$;

-- If the CfP for a program changes, it can affect the observation validations, so we
-- need to recalculate them all.
CREATE OR REPLACE FUNCTION cfp_assignment_obscalc_invalidate()
  RETURNS TRIGGER AS $$
BEGIN
  IF NEW.c_cfp_id IS DISTINCT FROM OLD.c_cfp_id THEN
    CALL invalidate_all_obscalc_for_program(NEW.c_program_id);
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER cfp_assignment_invalidate_obscalc_trigger
  AFTER UPDATE ON t_proposal
  FOR EACH ROW
  EXECUTE FUNCTION cfp_assignment_obscalc_invalidate();

-- Changes to a configuration request can affect the workflow validations of the associated
-- observations, so we need to recalculate them. Since there is no direct connection between
-- a configuration request and an observation, and it is difficult to calculate here, we'll
-- use a big hammer and just invalidate all obscalc results for the program. Updates to 
-- configuration requests are infrequent, so this shouldn't be a big deal.
CREATE OR REPLACE FUNCTION configreq_obscalc_invalidate()
  RETURNS TRIGGER AS $$
DECLARE
  configreq record;
BEGIN
  IF ROW(NEW.*) IS DISTINCT FROM ROW(OLD.*) THEN
    configreq := COALESCE(NEW, OLD);
    CALL invalidate_all_obscalc_for_program(configreq.c_program_id);
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER configreq_invalidate_obscalc_trigger
  AFTER INSERT OR UPDATE OR DELETE ON t_configuration_request
  FOR EACH ROW
  EXECUTE FUNCTION configreq_obscalc_invalidate();

-- The workflow validations depend on the CfP Ra/Dec and instrument, so we need to
-- invalidate them when the CfP changes. This should be very infrequent.

-- Invalidate the obscalc results for all observations in programs that are assigned the CfP
CREATE OR REPLACE PROCEDURE invalidate_all_obscalc_for_cfp(
  cfp_id d_cfp_id
)
  LANGUAGE plpgsql AS $$
DECLARE
  pid d_program_id;
BEGIN
  FOR pid IN
    SELECT c_program_id
    FROM t_proposal
    WHERE c_cfp_id = cfp_id
  LOOP
    CALL invalidate_all_obscalc_for_program(pid);
  END LOOP;
END;
$$;

CREATE OR REPLACE FUNCTION cfp_edit_obscalc_invalidate()
  RETURNS TRIGGER AS $$
BEGIN
  IF ROW(NEW.*) IS DISTINCT FROM ROW(OLD.*) THEN
    CALL invalidate_all_obscalc_for_cfp(NEW.c_cfp_id);
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER cfp_edit_invalidate_obscalc_trigger
  AFTER UPDATE ON t_cfp
  FOR EACH ROW
  EXECUTE FUNCTION cfp_edit_obscalc_invalidate();

CREATE TRIGGER cfp_instrument_invalidate_obscalc_trigger
  AFTER INSERT OR UPDATE OR DELETE ON t_cfp_instrument
  FOR EACH ROW
  EXECUTE FUNCTION cfp_edit_obscalc_invalidate();
