
-- This adds code that can find and close holes in groups, which can be caused by hard-deleting
-- group elements (groups or observations) without renumbering subsequent elements ("closing holes").

-- Update this procedure to add the offending group id as an exception detail.
CREATE OR REPLACE PROCEDURE group_verify_indices(pid d_program_id) AS $$
DECLARE
  g d_group_id;
BEGIN
  WITH indices AS (
    SELECT c_parent_id AS c_group_id, c_parent_index as c_index
    FROM   t_group
    WHERE  c_program_id = pid
    UNION
    SELECT c_group_id, c_group_index
    FROM   t_observation
    WHERE  c_program_id = pid
  )
  SELECT
    c_group_id
  INTO g
  FROM
    indices
  GROUP BY
    c_group_id
  HAVING starts_at_zero_and_is_consecutive(array_agg(c_index order by c_index)) = false;
  IF FOUND THEN
    IF g IS NULL THEN
      RAISE EXCEPTION 'Index discontinuity detected in the top-level group.';
    ELSE
      RAISE EXCEPTION 'Index discontinuity detected in group %.', g USING DETAIL = g;
    END IF;
  END IF;
END;
$$ LANGUAGE plpgsql;

-- Find the first broken group in the specified program, if any.
CREATE OR REPLACE FUNCTION group_find_broken(pid d_program_id) RETURNS d_group_id AS $$
DECLARE
  msg VARCHAR;
BEGIN
  call group_verify_indices(pid);
  return null;
exception
  when others then 
    GET STACKED DIAGNOSTICS msg = PG_EXCEPTION_DETAIL;
  return msg::d_group_id;
END;
$$ LANGUAGE plpgsql;

-- Find the first hole in arr, if any.
CREATE OR REPLACE FUNCTION group_find_hole(arr int[]) RETURNS int AS $$
DECLARE
  curr_idx int := 0;
  i int;
BEGIN
  IF arr IS NOT NULL THEN
    FOREACH i IN ARRAY arr LOOP
      IF i != curr_idx THEN
        RETURN curr_idx;
      END IF;
      curr_idx := curr_idx + 1;
    END LOOP;
  END IF;
  RETURN null;
END;
$$ LANGUAGE plpgsql;

-- Select the indices of elements in the specified group, or null if no such elements exist.
CREATE OR REPLACE FUNCTION group_indices(gid d_group_id) RETURNS int[] AS $$
DECLARE
  res int[];
BEGIN
  WITH indices AS (
    SELECT c_parent_id AS c_group_id, c_parent_index as c_index
    FROM   t_group
    WHERE  c_parent_id = gid
    UNION
    SELECT c_group_id, c_group_index
    FROM   t_observation
    WHERE  c_group_id = gid
  )
  select array_agg(c_index order by c_index) into res from indices;
  return res;
END;
$$ LANGUAGE plpgsql;

-- Repair all discontinuities in the specified group.
CREATE OR REPLACE PROCEDURE group_repair(pid d_program_id, gid d_group_id) AS $$
DECLARE
  hole int;
BEGIN
  LOOP
    hole := group_find_hole(group_indices(gid));
    IF hole IS NULL THEN
      EXIT;
    ELSE
      RAISE NOTICE '  %: closing hole at %', gid, hole;
      CALL group_close_hole(pid, gid, hole::int2);
    END IF;
  END LOOP;
END;
$$ LANGUAGE plpgsql;

-- Repair the first broken group in every program, if any. Returns true if any work
-- was done, and false if the call was a no-op. Call this in a loop until it returns
-- false to be sure all groups have been repaired.
CREATE OR REPLACE FUNCTION group_repair_first() RETURNS BOOLEAN AS $$
DECLARE
  pid d_program_id;
  gid d_group_id;
  ret boolean = false;
BEGIN
  SET CONSTRAINTS ALL DEFERRED;
  FOR pid, gid IN
    SELECT c_program_id, group_find_broken(c_program_id)
    FROM t_program
    WHERE group_find_broken(c_program_id) IS NOT NULL
  LOOP
    RAISE NOTICE 'Discontinuity found in %/%', pid, gid;
    CALL group_repair(pid, gid);
    ret = true;
  END LOOP;
  RETURN ret;
END;
$$ LANGUAGE plpgsql;

-- Repair all broken groups.
CREATE OR REPLACE PROCEDURE group_repair_all() AS $$
DECLARE
  more boolean = true;
BEGIN
  RAISE NOTICE 'Repairing all open holes.';
  WHILE more LOOP
    more := group_repair_first();
  END LOOP;
  RAISE NOTICE 'Done.';
END;
$$ LANGUAGE plpgsql;

-- Update this procedure to work properly.
CREATE OR REPLACE PROCEDURE delete_calibrations_by_role(calib_role e_calibration_role)
LANGUAGE plpgsql AS $$
BEGIN

  SET CONSTRAINTS ALL DEFERRED;

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

  -- Clean up
  call group_repair_all();

END;
$$;

-- Run it now.
CALL group_repair_all();
