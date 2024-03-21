-- Fix the observation update trigger function from V0847.
-- Needs to compare with IS DISTINCT FROM instead of <>.
CREATE OR REPLACE FUNCTION update_program_reference_in_observation()
RETURNS TRIGGER AS $$
BEGIN
  IF NEW.c_program_reference IS DISTINCT FROM OLD.c_program_reference THEN
    UPDATE t_observation
    SET c_program_reference = NEW.c_program_reference
    WHERE c_program_id = NEW.c_program_id;
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;