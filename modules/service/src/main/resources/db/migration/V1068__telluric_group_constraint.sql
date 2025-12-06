-- Fix existing data: set calibration_role = 'telluric' for observations in telluric groups
-- Assume the first observation with null is science
UPDATE t_observation o
SET c_calibration_role = 'telluric'
WHERE o.c_calibration_role IS NULL
  AND o.c_existence = 'present'
  AND EXISTS (
    SELECT 1 FROM t_group g
    WHERE g.c_group_id = o.c_group_id
      AND g.c_system = true
      AND 'telluric' = ANY(g.c_calibration_roles)
  )
  AND o.c_group_index > (
    SELECT MIN(o2.c_group_index) -- first on the group is science
    FROM t_observation o2
    WHERE o2.c_group_id = o.c_group_id
      AND o2.c_calibration_role IS NULL
      AND o2.c_existence = 'present'
  );

-- In a telluric group: at most 1 science obs with (null calibRole)
CREATE OR REPLACE FUNCTION check_telluric_group_observations()
RETURNS TRIGGER AS $$
DECLARE
  group_is_telluric BOOLEAN;
  science_obs_count INTEGER;
  invalid_calib_count INTEGER;
BEGIN
  -- Only check if observation is in a group
  IF NEW.c_group_id IS NULL THEN
    RETURN NEW;
  END IF;

  -- Check if the observation's group is a telluric system group
  SELECT EXISTS (
    SELECT 1 FROM t_group g
    WHERE g.c_group_id = NEW.c_group_id
      AND g.c_system = true
      AND 'telluric' = ANY(g.c_calibration_roles)
  ) INTO group_is_telluric;

  IF group_is_telluric THEN
    -- count observations without calibration role
    SELECT COUNT(*) INTO science_obs_count
    FROM t_observation o
    WHERE o.c_group_id = NEW.c_group_id
      AND o.c_calibration_role IS NULL
      AND o.c_existence = 'present';

    -- check observations with a different role
    SELECT COUNT(*) INTO invalid_calib_count
    FROM t_observation o
    WHERE o.c_group_id = NEW.c_group_id
      AND o.c_calibration_role IS NOT NULL
      AND o.c_calibration_role != 'telluric'
      AND o.c_existence = 'present';

    IF science_obs_count > 1 THEN
      RAISE EXCEPTION 'Telluric group % must have at most one science observation, found %',
        NEW.c_group_id, science_obs_count;
    END IF;

    IF invalid_calib_count > 0 THEN
      RAISE EXCEPTION 'Telluric group % observations must have calibration_role = telluric, found % with wrong role',
        NEW.c_group_id, invalid_calib_count;
    END IF;
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER check_telluric_group_observations_trigger
AFTER INSERT OR UPDATE OF c_group_id, c_calibration_role, c_existence ON t_observation
DEFERRABLE INITIALLY DEFERRED
FOR EACH ROW
EXECUTE FUNCTION check_telluric_group_observations();
