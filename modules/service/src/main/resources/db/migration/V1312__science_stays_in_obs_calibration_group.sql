-- A per-observation calibration group is found by looking for the system group
-- that contains its science observation.  If the science observation ends up
-- outside that group while the group still exists, the next recalculation sees
-- no group, tries to create one with the same name, and fails on
-- unique_system_group_name forever (the queue keeps retrying).
--
-- This trigger stops the science observation from leaving in the first place.
-- The one legitimate exit is the service's own cleanup, which deletes
-- the calibration observations first and only then moves the science
-- observation out before removing the group, so the move is allowed once the
-- group holds no calibration observations.

CREATE OR REPLACE FUNCTION check_science_leaves_obs_calibration_group()
RETURNS TRIGGER AS $$
DECLARE
  remaining int;
BEGIN
  IF OLD.c_group_id IS NOT NULL
     AND NEW.c_group_id IS DISTINCT FROM OLD.c_group_id
     AND OLD.c_calibration_role IS NULL
  THEN
    SELECT count(*)
    INTO   remaining
    FROM   t_observation o
    JOIN   t_group g ON g.c_group_id = o.c_group_id
    WHERE  o.c_group_id = OLD.c_group_id
      AND  o.c_observation_id <> OLD.c_observation_id
      AND  o.c_calibration_role IS NOT NULL
      AND  g.c_system
      AND  g.c_calibration_roles && '{telluric,daytime_pinhole}'::e_calibration_role[];

    IF remaining > 0 THEN
      RAISE EXCEPTION 'Observation % cannot leave its calibration group % while the group still holds calibration observations.',
        OLD.c_observation_id, OLD.c_group_id;
    END IF;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER science_leaves_obs_calibration_group_trigger
  BEFORE UPDATE OF c_group_id ON t_observation
  FOR EACH ROW
  EXECUTE FUNCTION check_science_leaves_obs_calibration_group();
