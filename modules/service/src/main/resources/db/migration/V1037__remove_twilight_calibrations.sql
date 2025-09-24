-- Create a general-purpose procedure for safely deleting calibration observations by role
-- This procedure deletes the dependencies
-- system programs also have calibration targets those should not be deleted
CREATE OR REPLACE PROCEDURE delete_calibrations_by_role(calib_role e_calibration_role)
LANGUAGE plpgsql AS $$
BEGIN
  -- Temporarily disable group constraint triggers to avoid discontinuity
  ALTER TABLE t_observation DISABLE TRIGGER group_trigger_observations;
  ALTER TABLE t_group DISABLE TRIGGER group_trigger_groups;

  -- First remove calibrations from groups,set existence to 'deleted' and remove from groups
  UPDATE t_observation
  SET c_existence = 'deleted',
      c_group_id = NULL,
      c_group_index = -1
  WHERE c_calibration_role = calib_role;

  -- Delete asterism target references for the calibration
  DELETE FROM t_asterism_target
  WHERE (c_program_id, c_observation_id) IN (
    SELECT c_program_id, c_observation_id
    FROM t_observation
    WHERE c_calibration_role = calib_role
  );

  -- Delete only calibration targets that were associated with the calibration observations we're deleting
  DELETE FROM t_target
  WHERE c_target_disposition = 'calibration'
    AND c_target_id IN (
      -- Get target associated with calibration observations
      SELECT DISTINCT at.c_target_id
      FROM t_asterism_target at
      JOIN t_observation o ON at.c_program_id = o.c_program_id AND at.c_observation_id = o.c_observation_id
      WHERE o.c_calibration_role = calib_role
    );

  -- Finally delete the calibration observations
  DELETE FROM t_observation
  WHERE c_calibration_role = calib_role;

  -- Re-enable group constraint triggers
  ALTER TABLE t_observation ENABLE TRIGGER group_trigger_observations;
  ALTER TABLE t_group ENABLE TRIGGER group_trigger_groups;
END;
$$;

-- Add trigger to prevent observations from being created in system programs
CREATE OR REPLACE FUNCTION check_no_observations_in_system_programs()
RETURNS TRIGGER AS $$
BEGIN
  IF EXISTS (
    SELECT 1 FROM t_program
    WHERE c_program_id = NEW.c_program_id
    AND c_program_type = 'system'
  ) THEN
    RAISE EXCEPTION 'Cannot create observations in system programs';
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trigger_check_no_observations_in_system_programs
  BEFORE INSERT OR UPDATE ON t_observation
  FOR EACH ROW
  EXECUTE FUNCTION check_no_observations_in_system_programs();

-- Delete twilight calibration observations they will be regenerated
CALL delete_calibrations_by_role('twilight');
