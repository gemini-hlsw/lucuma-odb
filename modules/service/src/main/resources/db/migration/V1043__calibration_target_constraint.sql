-- Ensure twilight, spectrophotometric, and telluric calibrations must have at least one target

-- Step 1: Delete existing untargeted calibrations before adding the constraint
-- pattern from V1037: disable triggers, remove from groups, then delete
ALTER TABLE t_observation DISABLE TRIGGER group_trigger_observations;
ALTER TABLE t_group DISABLE TRIGGER group_trigger_groups;

UPDATE t_observation
SET c_group_id = NULL,
    c_group_index = -1
WHERE c_calibration_role IN ('twilight', 'spectrophotometric', 'telluric', 'photometric')
  AND NOT EXISTS (
    SELECT 1
    FROM t_asterism_target at
    WHERE at.c_program_id = t_observation.c_program_id
      AND at.c_observation_id = t_observation.c_observation_id
  );

DELETE FROM t_observation
WHERE c_calibration_role IN ('twilight', 'spectrophotometric', 'telluric', 'photometric')
  AND NOT EXISTS (
    SELECT 1
    FROM t_asterism_target at
    WHERE at.c_program_id = t_observation.c_program_id
      AND at.c_observation_id = t_observation.c_observation_id
  );

ALTER TABLE t_observation ENABLE TRIGGER group_trigger_observations;
ALTER TABLE t_group ENABLE TRIGGER group_trigger_groups;

-- Step 2: Add trigger to validate calibration observations have targets when committed
CREATE OR REPLACE FUNCTION validate_calibration_has_target()
RETURNS TRIGGER AS $$
DECLARE
  target_count INTEGER;
  current_existence e_existence;
  current_calib_role e_calibration_role;
BEGIN
  -- This handles cases where the observation might be deleted after the update
  SELECT c_existence, c_calibration_role
  INTO current_existence, current_calib_role
  FROM t_observation
  WHERE c_program_id = NEW.c_program_id
    AND c_observation_id = NEW.c_observation_id;

  IF NOT FOUND OR current_existence = 'deleted' THEN
    RETURN NEW;
  END IF;

  -- Only check for twilight, spectrophotometric, and telluric calibrations
  IF current_calib_role IN ('twilight', 'spectrophotometric', 'telluric', 'photometric') THEN

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

-- Use a DEFERRABLE CONSTRAINT trigger to allow temporary inconsistent states
CREATE CONSTRAINT TRIGGER validate_calibration_target_trigger
AFTER INSERT OR UPDATE ON t_observation
DEFERRABLE INITIALLY DEFERRED
FOR EACH ROW
EXECUTE FUNCTION validate_calibration_has_target();
