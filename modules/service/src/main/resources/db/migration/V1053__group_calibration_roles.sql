-- Add calibration roles support to groups

ALTER TABLE t_group
ADD COLUMN c_calibration_roles e_calibration_role[] NOT NULL DEFAULT '{}'::e_calibration_role[];

-- Only system groups can have calibration roles
ALTER TABLE t_group
ADD CONSTRAINT group_calibration_roles_system_only_check
CHECK (c_system = true OR c_calibration_roles = '{}');

-- Recreate the v_group view to include the new column
DROP VIEW v_group;

CREATE VIEW v_group AS
  SELECT *,
  CASE WHEN c_min_interval IS NOT NULL THEN c_group_id END AS c_min_interval_id,
  CASE WHEN c_max_interval IS NOT NULL THEN c_group_id END AS c_max_interval_id
  FROM t_group;

-- Update existing "Calibrations" groups to have twilight and spectrophotometric roles
UPDATE t_group
SET c_calibration_roles = '{twilight,spectrophotometric}'::e_calibration_role[]
WHERE c_name = 'Calibrations'
  AND c_system = true
  AND c_calibration_roles = '{}'::e_calibration_role[];

-- Add existing F2 long slit science observations into system groups
DO $$
DECLARE
  obs_rec RECORD;
  new_group_id d_group_id;
  parent_group_id d_group_id;
  next_parent_index INT;
BEGIN
  -- Defer constraint triggers to allow temporary index discontinuities
  SET CONSTRAINTS ALL DEFERRED;

  -- Get each F2 science
  FOR obs_rec IN
    SELECT o.c_observation_id, o.c_program_id, o.c_group_id, o.c_group_index, f2.c_observing_mode_type
    FROM t_observation o
    INNER JOIN t_flamingos_2_long_slit f2
      ON o.c_observation_id = f2.c_observation_id
    WHERE
      o.c_calibration_role IS NULL
      -- Only process non-deleted observations
      AND o.c_existence = 'present'
      -- Not in a deleted group
      AND (o.c_group_id IS NULL OR EXISTS (
        SELECT 1 FROM t_group g
        WHERE g.c_group_id = o.c_group_id
          AND g.c_existence = 'present'
      ))
      -- Not already in a telluric system group
      AND NOT EXISTS (
        SELECT 1 FROM t_group g
        WHERE g.c_group_id = o.c_group_id
          AND g.c_system = true
          AND 'telluric' = ANY(g.c_calibration_roles)
      )
  LOOP
    -- preserve current group as parent if exists
    parent_group_id := obs_rec.c_group_id;

    -- Use the observation's current index so the telluric group takes its place
    -- Open a hole at this position (shifts everything forward)
    next_parent_index := group_open_hole(obs_rec.c_program_id, parent_group_id, obs_rec.c_group_index);

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
      -- Match naming convention in the code
      obs_rec.c_observing_mode_type::text || '/' || 'telluric' || '/' || obs_rec.c_observation_id,
      NULL,
      NULL,
      true,
      NULL,
      '0 seconds'::interval,
      'present',
      true,
      '{telluric}'::e_calibration_role[]
    ) RETURNING c_group_id INTO new_group_id;

    -- Use group_move_observation to properly close the hole and move the observation
    PERFORM group_move_observation(obs_rec.c_observation_id, new_group_id, 0::int2);

  END LOOP;
END $$;
