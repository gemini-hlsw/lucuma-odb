-- Add calibration roles support to groups
-- This allows system groups to indicate which types of calibrations they manage

-- Add the calibration roles column
ALTER TABLE t_group
ADD COLUMN c_calibration_roles e_calibration_role[] NOT NULL DEFAULT '{}'::e_calibration_role[];

COMMENT ON COLUMN t_group.c_calibration_roles IS
'Calibration roles supported by this group (system groups only). Empty array for non-calibration groups. Used to indicate which types of calibrations this group manages (e.g., Telluric, SpectroPhotometric). Only settable by system code, not user-editable.';

-- Recreate the v_group view to include the new column
DROP VIEW v_group;

CREATE VIEW v_group AS
  SELECT *,
  CASE WHEN c_min_interval IS NOT NULL THEN c_group_id END AS c_min_interval_id,
  CASE WHEN c_max_interval IS NOT NULL THEN c_group_id END AS c_max_interval_id
  FROM t_group;

-- Migrate existing F2 long slit science observations into system groups
-- This ensures all F2 observations are properly grouped for telluric calibration generation
DO $$
DECLARE
  obs_rec RECORD;
  new_group_id d_group_id;
  parent_group_id d_group_id;
  next_parent_index INT;
BEGIN
  -- Temporarily disable ALL triggers to avoid constraint issues during migration
  ALTER TABLE t_observation DISABLE TRIGGER ALL;
  ALTER TABLE t_group DISABLE TRIGGER ALL;

  -- Process each F2 long slit science observation
  FOR obs_rec IN
    SELECT o.c_observation_id, o.c_program_id, o.c_group_id, o.c_group_index
    FROM t_observation o
    INNER JOIN t_flamingos_2_long_slit f2
      ON o.c_observation_id = f2.c_observation_id
    WHERE
      -- Only science observations (not calibrations)
      o.c_calibration_role IS NULL
      -- Not already in a system group with telluric role
      AND NOT EXISTS (
        SELECT 1 FROM t_group g
        WHERE g.c_group_id = o.c_group_id
          AND g.c_system = true
          AND 'telluric' = ANY(g.c_calibration_roles)
      )
  LOOP
    -- Determine parent group (preserve current group as parent if exists)
    parent_group_id := obs_rec.c_group_id;

    -- Calculate next available index in parent group
    IF parent_group_id IS NOT NULL THEN
      SELECT COALESCE(MAX(c_parent_index) + 1, 0)
      INTO next_parent_index
      FROM t_group
      WHERE c_parent_id = parent_group_id;
    ELSE
      -- If no parent, this will be a top-level group
      SELECT COALESCE(MAX(c_parent_index) + 1, 0)
      INTO next_parent_index
      FROM t_group
      WHERE c_program_id = obs_rec.c_program_id
        AND c_parent_id IS NULL;
    END IF;

    -- Create new system group for this observation
    INSERT INTO t_group (
      c_program_id,
      c_parent_id,
      c_parent_index,
      c_name,
      c_description,
      c_min_required,
      c_ordered,
      c_min_interval,
      c_max_interval,
      c_existence,
      c_system,
      c_calibration_roles
    ) VALUES (
      obs_rec.c_program_id,
      parent_group_id,
      next_parent_index,
      'F2 Science and Telluric Standard for ' || obs_rec.c_observation_id,
      'System group for F2 observation and its telluric calibration',
      NULL,
      false,
      NULL,
      NULL,
      'present',
      true,
      '{telluric}'::e_calibration_role[]
    ) RETURNING c_group_id INTO new_group_id;

    -- Move observation to the new system group
    UPDATE t_observation
    SET c_group_id = new_group_id,
        c_group_index = 0
    WHERE c_observation_id = obs_rec.c_observation_id;

  END LOOP;

  -- Re-enable ALL triggers
  ALTER TABLE t_observation ENABLE TRIGGER ALL;
  ALTER TABLE t_group ENABLE TRIGGER ALL;
END $$;
