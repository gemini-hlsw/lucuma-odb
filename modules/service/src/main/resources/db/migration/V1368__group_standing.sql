-- Group standing: whether the groups around a group or observation still want
-- work in it.  It is viable, or complete or unschedulable as decided by the
-- outermost such group (the group itself included).  An observation stands as
-- its group does, and one outside any group is viable.
--
-- The standing is derived from group progress, which is evaluated in Scala, so
-- it is stored here for filtering and kept up to date by the obscalc daemon.
-- A group's standing depends on its whole top-level tree, so the tree is the
-- unit of calculation.
--
-- Edits that may change a standing add a mark naming the tree's top-level
-- group, and the daemon recalculates the marked trees.  Marking only ever
-- inserts a row, so it takes no lock an edit or an obscalc result could wait
-- on.  Correctness comes from visibility instead: the daemon deletes the marks
-- it can see, all of them committed, and then reads the tree, so it sees
-- their effects.  A mark committed later survives, and the tree is
-- recalculated again.

CREATE TYPE e_group_standing AS ENUM ('viable', 'complete', 'unschedulable');

-- Marks naming trees to recalculate, by top-level group.  There may be many
-- per tree, and none refer to t_group: a mark for a group since deleted, or
-- no longer top-level, has the daemon remove what was stored for it.
CREATE TABLE t_group_standing_mark (
  c_mark_id       bigserial  NOT NULL PRIMARY KEY,
  c_root_group_id d_group_id NOT NULL
);

CREATE INDEX i_group_standing_mark_root ON t_group_standing_mark (c_root_group_id);

-- The last calculation of each top-level group's tree, written only by the
-- daemon.  After a failure the tree is retried at c_retry_at, backing off with
-- c_failure_count.  c_reevaluate_at is the earliest time passing alone could
-- change a standing: when an ongoing group's maximum interval, or the night
-- for a same-night group, runs out.
CREATE TABLE t_group_standing_calc (
  c_group_id      d_group_id NOT NULL PRIMARY KEY,
  c_retry_at      timestamp  NULL,
  c_failure_count int4       NOT NULL DEFAULT 0 CHECK (c_failure_count >= 0),
  c_reevaluate_at timestamp  NULL
);

-- The standing of each group, as last calculated with its tree.  Like the
-- marks, it doesn't refer to t_group; a deleted group's row is removed when
-- its tree is recalculated.
CREATE TABLE t_group_standing (
  c_group_id          d_group_id       NOT NULL PRIMARY KEY,
  c_root_group_id     d_group_id       NOT NULL REFERENCES t_group_standing_calc(c_group_id) ON DELETE CASCADE,
  c_standing          e_group_standing NOT NULL,
  c_deciding_group_id d_group_id       NULL,

  CONSTRAINT group_standing_deciding_check CHECK ((c_standing = 'viable') = (c_deciding_group_id IS NULL))
);

CREATE INDEX i_group_standing_root ON t_group_standing (c_root_group_id);

-- The top-level group of the tree holding the given group.
CREATE FUNCTION group_root(group_id d_group_id)
  RETURNS d_group_id
  LANGUAGE sql STABLE AS $$
    WITH RECURSIVE up AS (
      SELECT c_group_id, c_parent_id FROM t_group WHERE c_group_id = group_id
      UNION ALL
      SELECT g.c_group_id, g.c_parent_id FROM t_group g JOIN up ON g.c_group_id = up.c_parent_id
    )
    SELECT c_group_id FROM up WHERE c_parent_id IS NULL
  $$;

-- Marks the tree holding the given group, if any.
CREATE PROCEDURE mark_group_standing(
  group_id d_group_id
) LANGUAGE plpgsql AS $$
DECLARE
  root d_group_id;
BEGIN
  root := group_root(group_id);
  IF root IS NOT NULL THEN
    INSERT INTO t_group_standing_mark (c_root_group_id) VALUES (root);
  END IF;
END;
$$;

-- Group edits that change progress, or the shape of a tree.  A move marks the
-- trees it leaves and joins.  A top-level group that moves into another tree,
-- or is deleted, is marked itself, so that what was stored for it as a tree
-- of its own is removed.
CREATE FUNCTION group_standing_group_mark()
  RETURNS TRIGGER AS $$
BEGIN
  IF TG_OP IN ('UPDATE', 'DELETE') THEN
    IF OLD.c_parent_id IS NULL THEN
      INSERT INTO t_group_standing_mark (c_root_group_id) VALUES (OLD.c_group_id);
    ELSE
      CALL mark_group_standing(OLD.c_parent_id);
    END IF;
  END IF;
  IF TG_OP IN ('INSERT', 'UPDATE') THEN
    CALL mark_group_standing(NEW.c_group_id);
  END IF;
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER group_standing_group_mark_trigger
  AFTER INSERT OR DELETE OR UPDATE OF
    c_parent_id, c_parent_index, c_existence, c_min_required, c_ordered,
    c_min_interval, c_max_interval, c_same_night
  ON t_group
  FOR EACH ROW
  EXECUTE FUNCTION group_standing_group_mark();

-- Observation edits that change the progress of its groups: moves, deletion,
-- the user state (inactive observations are left out), the calibration role
-- (calibrations inherit their science observation's user state), the
-- instrument (its site) and the declared execution state.
CREATE FUNCTION group_standing_observation_mark()
  RETURNS TRIGGER AS $$
BEGIN
  IF TG_OP IN ('UPDATE', 'DELETE') AND OLD.c_group_id IS NOT NULL THEN
    CALL mark_group_standing(OLD.c_group_id);
  END IF;
  IF TG_OP IN ('INSERT', 'UPDATE') AND NEW.c_group_id IS NOT NULL THEN
    CALL mark_group_standing(NEW.c_group_id);
  END IF;
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER group_standing_observation_mark_trigger
  AFTER INSERT OR DELETE OR UPDATE OF
    c_group_id, c_group_index, c_existence, c_workflow_user_state,
    c_calibration_role, c_instrument, c_declared_state
  ON t_observation
  FOR EACH ROW
  EXECUTE FUNCTION group_standing_observation_mark();

-- Execution changes an observation's progress facts (its execution state,
-- start and completion times), and obscalc stores the execution states it
-- finds.  The start comes with the first execution event that isn't a slew,
-- acquisition included, which is when the state moves to ongoing; completion
-- moves it to completed.  So a stored result marks the tree holding the
-- observation only when an execution state changed.  Most results change
-- nothing that matters here, and marking on each would keep an executing
-- observation's tree pending.  (A completion time that moves without a state
-- change, from events after completion, waits for the next mark.)
CREATE FUNCTION group_standing_obscalc_mark()
  RETURNS TRIGGER AS $$
DECLARE
  group_id d_group_id;
BEGIN
  SELECT c_group_id INTO group_id FROM t_observation WHERE c_observation_id = NEW.c_observation_id;
  IF group_id IS NOT NULL THEN
    CALL mark_group_standing(group_id);
  END IF;
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER group_standing_obscalc_mark_trigger
  AFTER UPDATE OF c_acq_execution_state, c_sci_execution_state ON t_obscalc
  FOR EACH ROW
  WHEN (
    OLD.c_acq_execution_state IS DISTINCT FROM NEW.c_acq_execution_state OR
    OLD.c_sci_execution_state IS DISTINCT FROM NEW.c_sci_execution_state
  )
  EXECUTE FUNCTION group_standing_obscalc_mark();

-- The calculation state of a tree, given its standing row (NULL when never
-- calculated): pending while marked or never calculated, retry after a
-- failure, ready otherwise.
CREATE FUNCTION group_standing_calc_state(
  root     d_group_id,
  retry_at timestamp
) RETURNS e_calculation_state
  LANGUAGE sql STABLE AS $$
    SELECT CASE
      WHEN root IS NULL THEN 'pending'
      WHEN EXISTS (SELECT 1 FROM t_group_standing_mark m WHERE m.c_root_group_id = root) THEN 'pending'
      WHEN retry_at IS NOT NULL THEN 'retry'
      ELSE 'ready'
    END :: e_calculation_state
  $$;

-- The standing of each present group: viable and pending until its tree is
-- first calculated.
CREATE VIEW v_group_standing AS
  SELECT
    g.c_group_id,
    COALESCE(s.c_standing, 'viable' :: e_group_standing) AS c_standing,
    s.c_deciding_group_id,
    group_standing_calc_state(s.c_root_group_id, c.c_retry_at) AS c_calc_state
  FROM t_group g
  LEFT JOIN t_group_standing      s ON s.c_group_id = g.c_group_id
  LEFT JOIN t_group_standing_calc c ON c.c_group_id = s.c_root_group_id
  WHERE g.c_existence = 'present';

-- The standing of each observation: its group's, or viable (and nothing to
-- calculate) outside any group.
CREATE VIEW v_observation_group_standing AS
  SELECT
    o.c_observation_id,
    COALESCE(s.c_standing, 'viable' :: e_group_standing) AS c_standing,
    s.c_deciding_group_id,
    CASE
      WHEN o.c_group_id IS NULL THEN 'ready' :: e_calculation_state
      ELSE group_standing_calc_state(s.c_root_group_id, c.c_retry_at)
    END AS c_calc_state
  FROM t_observation o
  LEFT JOIN t_group_standing      s ON s.c_group_id = o.c_group_id
  LEFT JOIN t_group_standing_calc c ON c.c_group_id = s.c_root_group_id;

-- Calculate every existing tree.
INSERT INTO t_group_standing_mark (c_root_group_id)
  SELECT c_group_id FROM t_group WHERE c_parent_id IS NULL AND c_existence = 'present';
