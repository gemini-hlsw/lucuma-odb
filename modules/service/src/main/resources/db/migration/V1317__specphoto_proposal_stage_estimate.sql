-- Spectrophotometric standards are charged a flat time estimate while the
-- program's proposal has yet to be accepted.  See Generator.SpecPhotoTimeEstimate.
--
-- The generator reads the proposal status through its own query rather than
-- v_generator_params, but the stored obscalc results keep no hash of their
-- inputs, so they have to be invalidated explicitly when the status changes.

CREATE OR REPLACE PROCEDURE invalidate_specphoto_obscalc_for_program(
  pid d_program_id
)
  LANGUAGE plpgsql AS $$
DECLARE
  obs_id d_observation_id;
BEGIN
  FOR obs_id IN
    SELECT c_observation_id
    FROM   t_observation
    WHERE  c_program_id       = pid
      AND  c_existence        = 'present'
      AND  c_calibration_role = 'spectrophotometric'
  LOOP
    CALL invalidate_obscalc(obs_id);
  END LOOP;
END;
$$;

-- Accepting a proposal switches its spectrophotometric standards from the flat
-- estimate to the real one.
CREATE OR REPLACE FUNCTION proposal_status_obscalc_invalidate()
  RETURNS TRIGGER AS $$
BEGIN
  IF NEW.c_proposal_status IS DISTINCT FROM OLD.c_proposal_status THEN
    CALL invalidate_specphoto_obscalc_for_program(NEW.c_program_id);
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER proposal_status_obscalc_invalidate_trigger
  AFTER UPDATE ON t_program
  FOR EACH ROW
  EXECUTE FUNCTION proposal_status_obscalc_invalidate();

-- Adding or removing a proposal moves the program in and out of the proposal
-- stage entirely, which flips the same switch.
CREATE OR REPLACE FUNCTION proposal_existence_obscalc_invalidate()
  RETURNS TRIGGER AS $$
BEGIN
  CALL invalidate_specphoto_obscalc_for_program(
    COALESCE(NEW.c_program_id, OLD.c_program_id)
  );
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER proposal_existence_obscalc_invalidate_trigger
  AFTER INSERT OR DELETE ON t_proposal
  FOR EACH ROW
  EXECUTE FUNCTION proposal_existence_obscalc_invalidate();
