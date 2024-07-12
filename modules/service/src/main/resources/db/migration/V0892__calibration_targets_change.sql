-- Calibration programs' targets are marked with the same calibration type
CREATE OR REPLACE FUNCTION calibration_targets_on_calibration_programs()
  RETURNS trigger AS $$
DECLARE
    calibration_role e_calibration_role;
BEGIN
    -- Fetch the value from the source table
    SELECT c_calibration_role INTO calibration_role
    FROM t_program
    WHERE c_program_id = NEW.c_program_id;

    -- Only set NEW.c_calibration_role if calibration_role is not null
    IF calibration_role IS NOT NULL THEN
        NEW.c_calibration_role := calibration_role;
    END IF;

    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS ch_target_calibration_target ON t_target;
CREATE TRIGGER ch_target_calibration_target
  BEFORE INSERT OR UPDATE ON t_target
  FOR EACH ROW
  EXECUTE PROCEDURE calibration_targets_on_calibration_programs();
