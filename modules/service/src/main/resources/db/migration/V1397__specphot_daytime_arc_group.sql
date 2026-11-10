-- A GMOS spectrophotometric standard gets a daytime arc in a system group of
-- its own, nested in the program's Calibrations group.  daytime_arc joins the
-- per-observation role set, and the observation a per-observation group hangs
-- off may now be a spectrophotometric standard as well as a science observation.

CREATE OR REPLACE FUNCTION check_obs_calibration_group_observations()
RETURNS TRIGGER AS $$
DECLARE
  is_obs_calibration_group BOOLEAN;
  parent_obs_count         INTEGER;
  invalid_calib_count      INTEGER;
BEGIN
  IF NEW.c_group_id IS NULL THEN
    RETURN NEW;
  END IF;

  SELECT EXISTS (
    SELECT 1 FROM t_group g
    WHERE g.c_group_id = NEW.c_group_id
      AND g.c_system = true
      AND g.c_calibration_roles && ARRAY['telluric','daytime_pinhole','daytime_flat','daytime_arc']::e_calibration_role[]
  ) INTO is_obs_calibration_group;

  IF is_obs_calibration_group THEN
    SELECT COUNT(*) INTO parent_obs_count
    FROM t_observation o
    WHERE o.c_group_id = NEW.c_group_id
      AND (o.c_calibration_role IS NULL OR o.c_calibration_role = 'spectrophotometric')
      AND o.c_existence = 'present';

    SELECT COUNT(*) INTO invalid_calib_count
    FROM t_observation o
    WHERE o.c_group_id = NEW.c_group_id
      AND o.c_calibration_role IS NOT NULL
      AND NOT (o.c_calibration_role = ANY (ARRAY['spectrophotometric','telluric','daytime_pinhole','daytime_flat','daytime_arc']::e_calibration_role[]))
      AND o.c_existence = 'present';

    IF parent_obs_count > 1 THEN
      RAISE EXCEPTION 'Per-observation calibration group % must have at most one science observation or spectrophotometric standard, found %',
        NEW.c_group_id, parent_obs_count;
    END IF;

    IF invalid_calib_count > 0 THEN
      RAISE EXCEPTION 'Per-observation calibration group % observations must be telluric, daytime_pinhole, daytime_flat or daytime_arc, found % with an invalid role',
        NEW.c_group_id, invalid_calib_count;
    END IF;
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION check_no_groups_in_obs_calibration_group()
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
      AND g.c_calibration_roles && ARRAY['telluric','daytime_pinhole','daytime_flat','daytime_arc']::e_calibration_role[]
  ) INTO parent_is_obs_calibration_group;

  IF parent_is_obs_calibration_group THEN
    RAISE EXCEPTION 'Cannot create group inside per-observation calibration group %', NEW.c_parent_id;
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION check_science_leaves_obs_calibration_group()
RETURNS TRIGGER AS $$
DECLARE
  remaining int;
BEGIN
  IF OLD.c_group_id IS NOT NULL
     AND NEW.c_group_id IS DISTINCT FROM OLD.c_group_id
     AND (OLD.c_calibration_role IS NULL OR OLD.c_calibration_role = 'spectrophotometric')
  THEN
    SELECT count(*)
    INTO   remaining
    FROM   t_observation o
    JOIN   t_group g ON g.c_group_id = o.c_group_id
    WHERE  o.c_group_id = OLD.c_group_id
      AND  o.c_observation_id <> OLD.c_observation_id
      AND  o.c_calibration_role IS NOT NULL
      AND  o.c_calibration_role <> 'spectrophotometric'
      AND  g.c_system
      AND  g.c_calibration_roles && '{telluric,daytime_pinhole,daytime_flat,daytime_arc}'::e_calibration_role[];

    IF remaining > 0 THEN
      RAISE EXCEPTION 'Observation % cannot leave its calibration group % while the group still holds calibration observations.',
        OLD.c_observation_id, OLD.c_group_id;
    END IF;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION is_obs_calibration_group(gid d_group_id)
RETURNS BOOLEAN AS $$
  SELECT EXISTS (
    SELECT 1 FROM t_group g
    WHERE g.c_group_id = gid
      AND g.c_system = true
      AND g.c_calibration_roles && ARRAY['telluric','daytime_pinhole','daytime_flat','daytime_arc']::e_calibration_role[]
  );
$$ LANGUAGE sql STABLE;

-- Existing GMOS spectroscopy science observations need a calibration pass so
-- their spectrophotometric standards get a group and a daytime arc.
DO $$
DECLARE
  o RECORD;
BEGIN
  FOR o IN
    SELECT c_observation_id, c_program_id
    FROM   t_observation
    WHERE  c_observing_mode_type IN ('gmos_north_long_slit', 'gmos_south_long_slit', 'gmos_north_ifu', 'gmos_south_ifu')
      AND  c_calibration_role IS NULL
      AND  c_existence = 'present'
  LOOP
    CALL invalidate_calibration_calc(o.c_observation_id, o.c_program_id, 'recalc');
  END LOOP;
END $$;
