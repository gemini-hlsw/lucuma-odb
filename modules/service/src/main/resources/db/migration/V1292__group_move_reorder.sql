
-- Fix same-group reordering.
--
-- `group_move_*` removes the element (closing the hole it leaves behind) before inserting it
-- at the destination index, so the index it is given is interpreted against the list *after*
-- the element has been taken out. Clients compute the drop position against the list they are
-- looking at, which still contains the element being dragged, so every downward move within a
-- group landed one slot too low -- and a drop at the very bottom produced an index one past
-- the end, leaving a hole that the deferred group triggers rejected with
-- 'Index discontinuity detected ...'.
--
-- Two changes:
--
--   1. `group_move_observation` / `group_move_group` decrement the destination index when the
--      element is moving within its own group to a higher index, compensating for the shift
--      that closing the source hole already applied.
--
--   2. `group_open_hole` clamps the requested index to the end of the group instead of
--      assuming it is valid, and the movers use the value it returns. An out-of-range index
--      can no longer be turned into a hole, whatever the caller asks for.


-- Open a hole in the given program or program+group, at index i or at the end if i is null.
-- If i is beyond the end of the group it is clamped. Returns the index of the hole, which
-- may differ from i and which the caller must use.
-- Constraints must be deferred when calling.
CREATE OR REPLACE FUNCTION group_open_hole(pid d_program_id, gid d_group_id, i int2) RETURNS int2 AS $$
DECLARE
  ret int2;
BEGIN

  -- The next free slot at the end of the group. Elements parked at a negative index by
  -- group_move_group/group_move_observation are excluded; they are on their way elsewhere and
  -- are not part of the group's numbering.
  SELECT coalesce(max(c_index) + 1, 0) INTO ret
  FROM v_group_element
  WHERE c_group_id IS NOT DISTINCT FROM gid
  AND c_program_id = pid
  AND c_index >= 0;

  -- If we weren't given an index then append. No shuffling needed.
  IF i IS NULL THEN
    RETURN ret;
  END IF;

  -- Otherwise open a hole at i, clamped to the end of the group.
  ret := least(i, ret);

  -- Shuffle groups forward as needed, keeping in mind that gid may be null
  UPDATE t_group
  SET c_parent_index = c_parent_index + 1
  WHERE c_parent_id IS NOT DISTINCT FROM gid
  AND c_parent_index >= ret
  AND c_program_id = pid;

  -- Shuffle observations forward as needed
  UPDATE t_observation
  SET c_group_index = c_group_index + 1
  WHERE c_group_id IS NOT DISTINCT FROM gid
  AND c_group_index >= ret
  AND c_program_id = pid;

  RETURN ret;

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

    -- We're staying in the same group, so closing the hole above has already shifted
    -- everything past src_index down by one and the caller's index is one too high. A null
    -- dest and a null src both mean the top level, hence IS NOT DISTINCT FROM.
    IF dest IS NOT DISTINCT FROM src AND dest_index > src_index THEN
      dest_index := dest_index - 1;
    END IF;

    -- Open a hole where we're going, and take the (possibly clamped) index it gives us.
    dest_index := group_open_hole(pid, dest, dest_index);

  ELSE

    -- No need to open a hole but we do need to compute a real dest_index
    SELECT coalesce(max(c_index) + 1, 0) INTO dest_index
    FROM v_group_element
    WHERE c_group_id IS NOT DISTINCT FROM dest
    AND c_program_id = pid
    AND c_index >= 0;

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

    -- We're staying in the same group, so closing the hole above has already shifted
    -- everything past src_index down by one and the caller's index is one too high. A null
    -- dest and a null src_group both mean the top level, hence IS NOT DISTINCT FROM.
    IF dest IS NOT DISTINCT FROM src_group AND dest_index > src_index THEN
      dest_index := dest_index - 1;
    END IF;

    -- Open a hole where we're going, and take the (possibly clamped) index it gives us.
    dest_index := group_open_hole(pid, dest, dest_index);

  ELSE

    -- No need to open a hole but we do need to compute a real dest_index
    SELECT coalesce(max(c_index) + 1, 0) INTO dest_index
    FROM v_group_element
    WHERE c_group_id IS NOT DISTINCT FROM dest
    AND c_program_id = pid
    AND c_index >= 0;

  END IF;

  -- Finally put the observation where it goes.
  UPDATE t_observation
  SET    c_group_id = dest, c_group_index = dest_index
  WHERE  c_observation_id = oid;

END;
$$ LANGUAGE plpgsql;
