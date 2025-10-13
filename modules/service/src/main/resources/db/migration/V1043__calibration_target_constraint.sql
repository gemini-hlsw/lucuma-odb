-- Ensure twilight and spectrophotometric calibrations must have at least one target

-- Add trigger to validate calibration observations have targets when committed
-- This runs at the end of a transaction, allowing temporary states during updates
CREATE OR REPLACE FUNCTION validate_calibration_has_target()
RETURNS TRIGGER AS $$
DECLARE
  target_count INTEGER;
  current_existence e_existence;
  current_calib_role e_calibration_role;
BEGIN
  -- Re-read the current state of the observation at commit time
  -- This handles cases where the observation might be deleted after the update
  SELECT c_existence, c_calibration_role
  INTO current_existence, current_calib_role
  FROM t_observation
  WHERE c_program_id = NEW.c_program_id
    AND c_observation_id = NEW.c_observation_id;

  -- If observation no longer exists or was deleted, skip validation
  IF NOT FOUND OR current_existence = 'deleted' THEN
    RETURN NEW;
  END IF;

  -- Only check for twilight and spectrophotometric calibrations
  IF current_calib_role IN ('twilight', 'spectrophotometric') THEN

    -- Count targets in asterism for this observation
    SELECT COUNT(*) INTO target_count
    FROM t_asterism_target
    WHERE c_program_id = NEW.c_program_id
      AND c_observation_id = NEW.c_observation_id;

    -- Require at least one target
    IF target_count = 0 THEN
      RAISE EXCEPTION '% calibration observations must have at least one target (observation: %)',
        current_calib_role, NEW.c_observation_id;
    END IF;
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Use a CONSTRAINT trigger that is DEFERRABLE and runs at the end of transaction
-- This allows temporary inconsistent states during complex updates
CREATE CONSTRAINT TRIGGER validate_calibration_target_trigger
AFTER INSERT OR UPDATE ON t_observation
DEFERRABLE INITIALLY DEFERRED
FOR EACH ROW
EXECUTE FUNCTION validate_calibration_has_target();
