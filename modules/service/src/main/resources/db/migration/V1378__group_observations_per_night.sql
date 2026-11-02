-- Minimum and maximum observations per night for a group (see
-- notes/observations-per-night.md).  Null means no limit.
ALTER TABLE t_group
  ADD COLUMN c_min_obs_per_night int2 NULL CHECK (c_min_obs_per_night > 0),
  ADD COLUMN c_max_obs_per_night int2 NULL CHECK (c_max_obs_per_night > 0);

ALTER TABLE t_group
  ADD CONSTRAINT group_obs_per_night_order_check
  CHECK (c_min_obs_per_night IS NULL OR c_max_obs_per_night IS NULL OR c_min_obs_per_night <= c_max_obs_per_night);

-- The per-night limits are mutually exclusive with c_same_night.
ALTER TABLE t_group
  ADD CONSTRAINT group_obs_per_night_same_night_check
  CHECK (c_same_night = false OR (c_min_obs_per_night IS NULL AND c_max_obs_per_night IS NULL));

-- Recreate v_group so that its SELECT * picks up the new columns.
DROP VIEW v_group;

CREATE VIEW v_group AS
  SELECT *,
  CASE WHEN c_min_interval IS NOT NULL THEN c_group_id END AS c_min_interval_id,
  CASE WHEN c_max_interval IS NOT NULL THEN c_group_id END AS c_max_interval_id
  FROM t_group;

-- A group with per-night limits holds only observations and per-observation
-- calibration groups (a science observation wrapped with its calibrations).
-- Any other present subgroup is rejected.
CREATE FUNCTION is_obs_calibration_group(gid d_group_id)
RETURNS BOOLEAN AS $$
  SELECT EXISTS (
    SELECT 1 FROM t_group g
    WHERE g.c_group_id = gid
      AND g.c_system = true
      AND g.c_calibration_roles && ARRAY['telluric','daytime_pinhole']::e_calibration_role[]
  );
$$ LANGUAGE sql STABLE;

-- Deferred to commit, so it reads the row as it is then rather than trusting
-- NEW, which is the row as of the statement that queued the check.  The
-- createGroup and updateGroups mutations report the raised message to the
-- client as an inconsistent group error.
CREATE FUNCTION check_obs_per_night_subgroups()
RETURNS TRIGGER AS $$
DECLARE
  g t_group%ROWTYPE;
BEGIN
  SELECT * INTO g FROM t_group WHERE c_group_id = NEW.c_group_id;
  IF NOT FOUND THEN
    RETURN NULL;
  END IF;

  -- The group itself, when it has limits.
  IF (g.c_min_obs_per_night IS NOT NULL OR g.c_max_obs_per_night IS NOT NULL) AND EXISTS (
    SELECT 1 FROM t_group c
    WHERE c.c_parent_id = g.c_group_id
      AND c.c_existence = 'present'
      AND NOT is_obs_calibration_group(c.c_group_id)
  ) THEN
    RAISE EXCEPTION 'Group % has observations-per-night limits, so it cannot contain subgroups', g.c_group_id;
  END IF;

  -- The group's parent, when the parent has limits.
  IF g.c_parent_id IS NOT NULL
     AND g.c_existence = 'present'
     AND NOT is_obs_calibration_group(g.c_group_id)
     AND EXISTS (
       SELECT 1 FROM t_group p
       WHERE p.c_group_id = g.c_parent_id
         AND (p.c_min_obs_per_night IS NOT NULL OR p.c_max_obs_per_night IS NOT NULL)
     ) THEN
    RAISE EXCEPTION 'Group % has observations-per-night limits, so it cannot contain subgroups', g.c_parent_id;
  END IF;

  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER check_obs_per_night_subgroups_trigger
AFTER INSERT OR UPDATE OF c_parent_id, c_existence, c_system, c_calibration_roles, c_min_obs_per_night, c_max_obs_per_night ON t_group
DEFERRABLE INITIALLY DEFERRED
FOR EACH ROW
EXECUTE FUNCTION check_obs_per_night_subgroups();
