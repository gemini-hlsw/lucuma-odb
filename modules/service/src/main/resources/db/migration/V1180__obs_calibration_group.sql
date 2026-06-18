-- Rename the "telluric group" concept to the "per-observation calibration
-- group": a system group that holds a single science observation's
-- calibrations (tellurics and, for GNIRS cross-dispersed, daytime pinhole
-- flats).  The group is identified by being a per-observation calibration
-- container (its c_calibration_roles overlaps the per-observation role set
-- {telluric, daytime_pinhole}), not by containing a telluric specifically.
--
-- This supersedes the telluric-named constraint triggers/functions created in
-- V1070 (and V1180's earlier daytime_pinhole patch on this branch): drop them
-- and recreate generically-named ones with the array-overlap discriminant.

DROP TRIGGER  IF EXISTS check_telluric_group_observations_trigger ON t_observation;
DROP TRIGGER  IF EXISTS check_no_groups_in_telluric_group_trigger  ON t_group;
DROP FUNCTION IF EXISTS check_telluric_group_observations();
DROP FUNCTION IF EXISTS check_no_groups_in_telluric_group();

-- A per-observation calibration group holds at most one science observation
-- (null calibration role) plus calibrations whose roles are within the
-- per-observation set.
CREATE FUNCTION check_obs_calibration_group_observations()
RETURNS TRIGGER AS $$
DECLARE
  is_obs_calibration_group BOOLEAN;
  science_obs_count        INTEGER;
  invalid_calib_count      INTEGER;
BEGIN
  IF NEW.c_group_id IS NULL THEN
    RETURN NEW;
  END IF;

  SELECT EXISTS (
    SELECT 1 FROM t_group g
    WHERE g.c_group_id = NEW.c_group_id
      AND g.c_system = true
      AND g.c_calibration_roles && ARRAY['telluric','daytime_pinhole']::e_calibration_role[]
  ) INTO is_obs_calibration_group;

  IF is_obs_calibration_group THEN
    SELECT COUNT(*) INTO science_obs_count
    FROM t_observation o
    WHERE o.c_group_id = NEW.c_group_id
      AND o.c_calibration_role IS NULL
      AND o.c_existence = 'present';

    SELECT COUNT(*) INTO invalid_calib_count
    FROM t_observation o
    WHERE o.c_group_id = NEW.c_group_id
      AND o.c_calibration_role IS NOT NULL
      AND NOT (o.c_calibration_role = ANY (ARRAY['telluric','daytime_pinhole']::e_calibration_role[]))
      AND o.c_existence = 'present';

    IF science_obs_count > 1 THEN
      RAISE EXCEPTION 'Per-observation calibration group % must have at most one science observation, found %',
        NEW.c_group_id, science_obs_count;
    END IF;

    IF invalid_calib_count > 0 THEN
      RAISE EXCEPTION 'Per-observation calibration group % observations must be telluric or daytime_pinhole, found % with an invalid role',
        NEW.c_group_id, invalid_calib_count;
    END IF;
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER check_obs_calibration_group_observations_trigger
AFTER INSERT OR UPDATE OF c_group_id, c_calibration_role, c_existence ON t_observation
DEFERRABLE INITIALLY DEFERRED
FOR EACH ROW
EXECUTE FUNCTION check_obs_calibration_group_observations();

-- Prevent creating child groups inside a per-observation calibration group.
CREATE FUNCTION check_no_groups_in_obs_calibration_group()
RETURNS TRIGGER AS $$
DECLARE
  parent_is_obs_calibration_group BOOLEAN;
BEGIN
  IF NEW.c_parent_id IS NULL THEN
    RETURN NEW;
  END IF;

  SELECT EXISTS (
    SELECT 1 FROM t_group g
    WHERE g.c_group_id = NEW.c_parent_id
      AND g.c_system = true
      AND g.c_calibration_roles && ARRAY['telluric','daytime_pinhole']::e_calibration_role[]
  ) INTO parent_is_obs_calibration_group;

  IF parent_is_obs_calibration_group THEN
    RAISE EXCEPTION 'Cannot create group inside per-observation calibration group %', NEW.c_parent_id;
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER check_no_groups_in_obs_calibration_group_trigger
AFTER INSERT OR UPDATE OF c_parent_id ON t_group
DEFERRABLE INITIALLY DEFERRED
FOR EACH ROW
EXECUTE FUNCTION check_no_groups_in_obs_calibration_group();
