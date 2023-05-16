
-- This fixes a problem where moving things out of the top-level group
-- led to an index discontinuity error. We were failing to close the hole.


-- Shuffle things back to close a hole at the specified group+index. Used for move and delete.
-- Constraints must be deferred when calling.
CREATE OR REPLACE PROCEDURE group_close_hole(pid d_program_id, gid d_group_id, i int2) AS $$
BEGIN

  -- Shuffle groups back as needed, keeping in mind that gid may be null
  UPDATE t_group
  SET c_parent_index = c_parent_index - 1
  WHERE c_parent_id IS NOT DISTINCT FROM gid 
  AND c_parent_index > i
  AND c_program_id = pid;

  -- Shuffle observations back as needed
  UPDATE t_observation
  SET c_group_index = c_group_index - 1
  WHERE c_group_id IS NOT DISTINCT FROM gid
  AND c_group_index > i
  AND c_program_id = pid;

END;
$$ LANGUAGE plpgsql;


-- Move a group. Constraints must be deferred when calling.
CREATE OR REPLACE FUNCTION group_move_group(gid d_group_id, dest d_group_id, dest_index int2) RETURNS VOID AS $$
DECLARE
  pid d_program_id;
  src d_group_id;
  src_index int2;
BEGIN

  -- Get the current location
  SELECT c_program_id, c_parent_id, c_parent_index
  INTO   pid, src, src_index
  FROM   t_group
  WHERE  c_group_id = gid;

  -- Punt if not found
  IF NOT FOUND THEN
    RAISE EXCEPTION 'Group % was not found.', gid;
  END IF;

  -- Move it out of the way
  UPDATE t_group
  SET    c_parent_id = null, c_parent_index = -1
  WHERE  c_group_id = gid;

  -- Close the hole where used to be
  CALL group_close_hole(pid, src, src_index);


  IF dest_index IS NOT NULL THEN

    -- Open a hole where we're going.
    PERFORM group_open_hole(pid, dest, dest_index);

  ELSE

    -- No need to open a hole but we do need to compute a real dest_index 
    SELECT coalesce(max(c_index) + 1, 0) INTO dest_index
    FROM v_group_element
    WHERE c_group_id IS NOT DISTINCT FROM dest
    AND c_program_id = pid;

  END IF;

  -- Finally put the group where it goes.
  UPDATE t_group
  SET    c_parent_id = dest, c_parent_index = dest_index
  WHERE  c_group_id = gid;

END;
$$ LANGUAGE plpgsql;

-- Move an observation. Constraints must be deferred when calling.
CREATE OR REPLACE FUNCTION group_move_observation(oid d_observation_id, dest d_group_id, dest_index int2) RETURNS VOID AS $$
DECLARE
  pid d_program_id;
  src_group d_group_id;
  src_index int2;
BEGIN

  -- Get the current location
  SELECT c_program_id, c_group_id, c_group_index
  INTO   pid, src_group, src_index
  FROM   t_observation
  WHERE  c_observation_id = oid;

  -- Punt if not found
  IF NOT FOUND THEN
    RAISE EXCEPTION 'Observation % was not found.', oid;
  END IF;
  
  -- Move it out of the way
  UPDATE t_observation
  SET    c_group_id = null, c_group_index = -1
  WHERE  c_observation_id = oid;

  -- Close the hole where used to be
  CALL group_close_hole(pid, src_group, src_index);

  IF dest_index IS NOT NULL THEN

    -- Open a hole where we're going.
    PERFORM group_open_hole(pid, dest, dest_index);

  ELSE

    -- No need to open a hole but we do need to compute a real dest_index 
    SELECT coalesce(max(c_index) + 1, 0) INTO dest_index
    FROM v_group_element
    WHERE c_group_id IS NOT DISTINCT FROM dest
    AND c_program_id = pid;

  END IF;

  -- Finally put the observation where it goes.
  UPDATE t_observation
  SET    c_group_id = dest, c_group_index = dest_index
  WHERE  c_observation_id = oid;

END;
$$ LANGUAGE plpgsql;
