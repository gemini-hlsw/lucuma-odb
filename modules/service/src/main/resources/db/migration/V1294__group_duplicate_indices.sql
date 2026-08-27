-- Detect and repair duplicate group indices.
--
-- The elements of a group -- child groups in t_group (c_parent_id, c_parent_index) and
-- observations in t_observation (c_group_id, c_group_index) -- must together be numbered
-- 0..n-1 with no gaps and no repeats. Holes were handled by V1047; duplicates were not, and
-- could not be: `group_verify_indices` and `group_indices` combined the two tables with UNION
-- rather than UNION ALL, so two elements sharing a (parent, index) collapsed into one row and
-- `starts_at_zero_and_is_consecutive` saw a perfect run. The check runs on every write (see
-- the group_trigger_* constraint triggers) and duplicates went through it untouched.
--
-- Two elements sharing an index are not merely untidy. v_group_element.c_group_element_id is
-- 'program:parent:index' and the GraphQL layer uses it as the GroupElement key, so colliding
-- elements share a primary key and get collapsed into one -- elements silently disappear from
-- groupElements queries.
--
-- Changes:
--
--   1. `v_group_element` uses UNION ALL, and `group_indices` reads the view rather than
--      re-deriving the union. The view's two branches could never produce a duplicate row (the
--      child id columns differ), so the dedupe only hid corruption and cost a sort.
--
--   2. `group_verify_indices` tests validity with an aggregate instead of walking an array. A
--      set of n elements is correctly numbered iff min = 0, max = n-1, and the indices are
--      distinct -- one test that catches holes, duplicates, and a short tail (n elements whose
--      highest index is below n-1, which is what duplicates leave behind).
--
--   3. Repair renumbers instead of closing holes. `group_close_hole` cannot separate two
--      elements sharing a slot, so `group_repair` now calls the new `group_compact`, which
--      reassigns 0..n-1 over the group ordered by (current index, child id).
--
--   4. `group_repair_all` iterates the invalid groups directly and `group_repair_first` is
--      dropped. Besides being O(holes) round trips, `group_repair_first` could not terminate on
--      top-level breakage: `group_find_broken` reports the offending group through
--      PG_EXCEPTION_DETAIL, the top-level RAISE sets no DETAIL, and d_group_id is an
--      unconstrained varchar -- so it returned '' rather than NULL, passed the IS NOT NULL
--      filter, matched no elements to repair, and looped forever. Compaction is idempotent, so
--      a single pass suffices and the loop is gone.
--
--   5. `group_open_hole` takes a per-program advisory lock. It reads max(c_index) + 1 and the
--      caller then inserts at that index, so concurrent creates in one program could both be
--      handed the same slot -- which is how the duplicates in production arose, from separate
--      calibration recalculation transactions each numbering from its own snapshot. Follows the
--      pg_advisory_xact_lock precedent in VisitService. Every caller derives pid from the
--      element it is placing and so takes at most one of these locks per transaction; that is
--      what keeps a lock-ordering deadlock out of reach, so a future caller that places
--      elements in two programs at once would need to acquire them in a defined order.


-- Elements of every group, one row per element. UNION ALL: the two branches are disjoint by
-- construction (one always has a null child observation, the other a null child group), so
-- there is nothing for UNION to deduplicate except genuinely duplicated indices.
CREATE OR REPLACE VIEW v_group_element AS
  SELECT
    c_program_id || ':' || coalesce(c_parent_id, 'null') || ':' || c_parent_index AS c_group_element_id,
    c_program_id,
    c_parent_id AS c_group_id,
    c_parent_index AS c_index,
    c_group_id AS c_child_group_id,
    null::d_observation_id AS c_child_observation_id,
    c_existence
  FROM t_group
  UNION ALL
  SELECT
    c_program_id || ':' || coalesce(c_group_id, 'null') || ':' || c_group_index,
    c_program_id,
    c_group_id,
    c_group_index,
    null,  -- untyped, as in V0871: this resolves c_child_group_id to varchar, not d_group_id
    c_observation_id,
    c_existence
  FROM t_observation;


-- Select the indices of elements in the specified group, or null if no such elements exist.
--
-- Retained for diagnostics only; the repair path no longer uses it. Note that it takes no
-- program id, so it cannot describe a top level: `= gid` yields nothing when gid is null, and
-- widening that to IS NOT DISTINCT FROM would aggregate every program's top level into one
-- array. That blind spot is part of why the old hole-closing repair could never fix a top-level
-- group. Use v_group_invalid_indices instead, which is keyed by (program, group).
CREATE OR REPLACE FUNCTION group_indices(gid d_group_id) RETURNS int[] AS $$
DECLARE
  res int[];
BEGIN
  SELECT array_agg(c_index ORDER BY c_index) INTO res
  FROM   v_group_element
  WHERE  c_group_id = gid;
  RETURN res;
END;
$$ LANGUAGE plpgsql;


-- Every incorrectly numbered group in the database, with the numbering it actually has. A null
-- c_group_id means the top level of the program. Useful for diagnosis, and the source of truth
-- for group_repair_all below.
CREATE OR REPLACE VIEW v_group_invalid_indices AS
  SELECT
    c_program_id,
    c_group_id,
    count(*)                 AS c_elements,
    count(DISTINCT c_index)  AS c_distinct_indices,
    min(c_index)             AS c_min_index,
    max(c_index)             AS c_max_index
  FROM v_group_element
  GROUP BY c_program_id, c_group_id
  HAVING count(*) <> count(DISTINCT c_index)   -- two elements sharing an index
      OR min(c_index) <> 0                     -- does not start at zero
      OR max(c_index) <> count(*) - 1;         -- a hole, or a tail lost to duplicates


-- Raise an error if any group in the specified program is incorrectly numbered. Reports the
-- offending group id as an exception detail, which group_find_broken reads back.
CREATE OR REPLACE PROCEDURE group_verify_indices(pid d_program_id) AS $$
DECLARE
  g   d_group_id;
  n   bigint;
  d   bigint;
  lo  int;
  hi  int;
  what text;
  msg text;
BEGIN

  SELECT c_group_id, c_elements, c_distinct_indices, c_min_index, c_max_index
  INTO   g, n, d, lo, hi
  FROM   v_group_invalid_indices
  WHERE  c_program_id = pid
  LIMIT  1;

  IF FOUND THEN

    what := coalesce('group ' || g, 'the top-level group');

    IF n <> d THEN
      msg := format('Duplicate index detected in %s: %s elements occupy %s distinct indices.', what, n, d);
    ELSE
      msg := format('Index discontinuity detected in %s: %s elements numbered %s..%s.', what, n, lo, hi);
    END IF;

    -- The top level has no group id to report, and group_find_broken cannot distinguish an
    -- empty detail from a real id, so raise without one in that case.
    IF g IS NULL THEN
      RAISE EXCEPTION '%', msg;
    ELSE
      RAISE EXCEPTION '%', msg USING DETAIL = g;
    END IF;

  END IF;

END;
$$ LANGUAGE plpgsql;


-- Renumber the elements of the specified group 0..n-1, ordered by their current index and then
-- by child id to break ties deterministically. This is the general repair: it closes holes,
-- separates elements that share an index, and pulls in a short tail. Constraints must be
-- deferred when calling.
CREATE OR REPLACE PROCEDURE group_compact(pid d_program_id, gid d_group_id) AS $$
BEGIN

  -- Both updates read `ordered`, which is computed from the statement's snapshot, so the
  -- renumbering of groups below cannot perturb the numbering computed for observations. The
  -- inequality guards keep untouched rows from firing their triggers.
  WITH ordered AS (
    SELECT
      c_child_group_id,
      c_child_observation_id,
      (row_number() OVER (
         ORDER BY c_index, coalesce(c_child_group_id::text, c_child_observation_id::text)
       ) - 1)::int2 AS c_new_index
    FROM v_group_element
    WHERE c_program_id = pid
    AND   c_group_id IS NOT DISTINCT FROM gid
  ),
  g AS (
    UPDATE t_group t
    SET    c_parent_index = o.c_new_index
    FROM   ordered o
    WHERE  t.c_group_id = o.c_child_group_id
    AND    t.c_parent_index <> o.c_new_index
    RETURNING 1
  )
  UPDATE t_observation t
  SET    c_group_index = o.c_new_index
  FROM   ordered o
  WHERE  t.c_observation_id = o.c_child_observation_id
  AND    t.c_group_index <> o.c_new_index;

END;
$$ LANGUAGE plpgsql;


-- Repair the numbering of the specified group.
CREATE OR REPLACE PROCEDURE group_repair(pid d_program_id, gid d_group_id) AS $$
BEGIN
  CALL group_compact(pid, gid);
END;
$$ LANGUAGE plpgsql;


-- group_repair_all no longer needs this, and it cannot terminate on top-level breakage. See
-- the header.
DROP FUNCTION IF EXISTS group_repair_first();


-- Repair every incorrectly numbered group. Compaction is idempotent and cannot introduce a new
-- defect, so one pass over the invalid groups is enough.
CREATE OR REPLACE PROCEDURE group_repair_all() AS $$
DECLARE
  r record;
  n int := 0;
BEGIN

  SET CONSTRAINTS ALL DEFERRED;

  FOR r IN SELECT * FROM v_group_invalid_indices LOOP
    RAISE NOTICE 'Compacting %/% (% elements, % distinct indices, numbered %..%)',
      r.c_program_id, coalesce(r.c_group_id, '<top level>'),
      r.c_elements, r.c_distinct_indices, r.c_min_index, r.c_max_index;
    CALL group_compact(r.c_program_id, r.c_group_id);
    n := n + 1;
  END LOOP;

  RAISE NOTICE 'Repaired % group(s).', n;

END;
$$ LANGUAGE plpgsql;


-- Open a hole in the given program or program+group, at index i or at the end if i is null.
-- If i is beyond the end of the group it is clamped. Returns the index of the hole, which
-- may differ from i and which the caller must use.
-- Constraints must be deferred when calling.
CREATE OR REPLACE FUNCTION group_open_hole(pid d_program_id, gid d_group_id, i int2) RETURNS int2 AS $$
DECLARE
  ret int2;
BEGIN

  -- The next free slot is read and then handed back to the caller, which inserts at it, so
  -- concurrent creates in one program have to be serialized or they are handed the same slot.
  -- The lock is per program and released at commit.
  PERFORM pg_advisory_xact_lock(hashtext(pid::text)::bigint);

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


-- Clean up everything the old check could not see.
CALL group_repair_all();
