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

-- The top-level group of the tree holding the given group.  A move may leave
-- the groups in a cycle until the end of its transaction, when the deferred
-- group_verify_acyclic rejects it, and the mark triggers run before then.
-- UNION, unlike UNION ALL, stops at a row it has already seen, so the walk
-- ends there and finds no root.
CREATE FUNCTION group_root(group_id d_group_id)
  RETURNS d_group_id
  LANGUAGE sql STABLE AS $$
    WITH RECURSIVE up AS (
      SELECT c_group_id, c_parent_id FROM t_group WHERE c_group_id = group_id
      UNION
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

-- Marks the tree holding the given observation, if it is in a group.
CREATE PROCEDURE mark_observation_group_standing(
  observation_id d_observation_id
) LANGUAGE plpgsql AS $$
DECLARE
  group_id d_group_id;
BEGIN
  SELECT c_group_id INTO group_id FROM t_observation WHERE c_observation_id = observation_id;
  IF group_id IS NOT NULL THEN
    CALL mark_group_standing(group_id);
  END IF;
END;
$$;

-- Group edits that change progress, or the shape of a tree.  A move marks the
-- trees it leaves and joins.  A top-level group that moves into another tree,
-- or is deleted, is marked itself, so that what was stored for it as a tree
-- of its own is removed.  An update that writes the columns without changing
-- them marks nothing.
CREATE FUNCTION group_standing_group_mark()
  RETURNS TRIGGER AS $$
BEGIN
  IF TG_OP = 'UPDATE' AND
     (OLD.c_parent_id, OLD.c_parent_index, OLD.c_existence, OLD.c_min_required,
      OLD.c_ordered, OLD.c_min_interval, OLD.c_max_interval, OLD.c_same_night,
      OLD.c_min_obs_per_night, OLD.c_max_obs_per_night)
     IS NOT DISTINCT FROM
     (NEW.c_parent_id, NEW.c_parent_index, NEW.c_existence, NEW.c_min_required,
      NEW.c_ordered, NEW.c_min_interval, NEW.c_max_interval, NEW.c_same_night,
      NEW.c_min_obs_per_night, NEW.c_max_obs_per_night) THEN
    RETURN NULL;
  END IF;
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
    c_min_interval, c_max_interval, c_same_night, c_min_obs_per_night,
    c_max_obs_per_night
  ON t_group
  FOR EACH ROW
  EXECUTE FUNCTION group_standing_group_mark();

-- Observation edits that change the progress of its groups: moves, deletion,
-- the user state (inactive observations are left out), the calibration role
-- (calibrations inherit their science observation's user state), the
-- instrument (its site), the declared execution state and the QA override
-- (a complete observation that didn't pass QA is rejected).  An update that
-- writes the columns without changing them marks nothing.
CREATE FUNCTION group_standing_observation_mark()
  RETURNS TRIGGER AS $$
BEGIN
  IF TG_OP = 'UPDATE' AND
     (OLD.c_group_id, OLD.c_group_index, OLD.c_existence, OLD.c_workflow_user_state,
      OLD.c_calibration_role, OLD.c_instrument, OLD.c_declared_state, OLD.c_qa_state_override)
     IS NOT DISTINCT FROM
     (NEW.c_group_id, NEW.c_group_index, NEW.c_existence, NEW.c_workflow_user_state,
      NEW.c_calibration_role, NEW.c_instrument, NEW.c_declared_state, NEW.c_qa_state_override) THEN
    RETURN NULL;
  END IF;
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
    c_calibration_role, c_instrument, c_declared_state, c_qa_state_override
  ON t_observation
  FOR EACH ROW
  EXECUTE FUNCTION group_standing_observation_mark();

-- Marks the tree holding the row's observation (NEW.c_observation_id), for
-- tables keyed by observation whose triggers decide when.
CREATE FUNCTION group_standing_observation_id_mark()
  RETURNS TRIGGER AS $$
BEGIN
  CALL mark_observation_group_standing(NEW.c_observation_id);
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

-- Staff suspending or resuming an observation leaves it out of its groups or
-- brings it back, along with its per-observation calibrations, which are in
-- the same group.
CREATE TRIGGER group_standing_suspension_mark_trigger
  AFTER UPDATE OF c_suspended ON t_observation_execution
  FOR EACH ROW
  WHEN (OLD.c_suspended IS DISTINCT FROM NEW.c_suspended)
  EXECUTE FUNCTION group_standing_observation_id_mark();

-- A dataset QA change, or a dataset being written, can move its observation's
-- QA state (v_observation_qa).  Group progress reads the QA state only once
-- the observation is complete, so only then does it mark the tree: datasets
-- written and assessed while the observation executes would otherwise keep
-- its tree pending throughout.  Completion itself marks the tree (below), and
-- the refresh then reads the QA as it is, so nothing is missed while obscalc
-- has yet to store the completion.  A bulk QA edit marks the tree once per
-- dataset, and the refresh claims the marks together.
CREATE FUNCTION group_standing_dataset_mark()
  RETURNS TRIGGER AS $$
BEGIN
  IF EXISTS (
    SELECT 1
      FROM t_obscalc c
     WHERE c.c_observation_id = NEW.c_observation_id
       AND c.c_workflow_state = 'completed'::e_workflow_state
  ) THEN
    CALL mark_observation_group_standing(NEW.c_observation_id);
  END IF;
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER group_standing_dataset_mark_trigger
  AFTER UPDATE OF c_qa_state, c_end_time ON t_dataset
  FOR EACH ROW
  WHEN (
    OLD.c_qa_state IS DISTINCT FROM NEW.c_qa_state OR
    OLD.c_end_time IS DISTINCT FROM NEW.c_end_time
  )
  EXECUTE FUNCTION group_standing_dataset_mark();

-- Execution changes an observation's progress facts (its execution state,
-- start and completion times), and obscalc stores the execution states it
-- finds.  The start comes with the first execution event that isn't a slew,
-- acquisition included, which is when the state moves to ongoing; completion
-- moves it to completed.  So a stored result marks the tree holding the
-- observation only when an execution state changed.  Most results change
-- nothing that matters here, and marking on each would keep an executing
-- observation's tree pending.  (A completion time that moves without a state
-- change, from events after completion, waits for the next mark, except a
-- visitor observation's, below.)
CREATE TRIGGER group_standing_obscalc_mark_trigger
  AFTER UPDATE OF c_acq_execution_state, c_sci_execution_state ON t_obscalc
  FOR EACH ROW
  WHEN (
    OLD.c_acq_execution_state IS DISTINCT FROM NEW.c_acq_execution_state OR
    OLD.c_sci_execution_state IS DISTINCT FROM NEW.c_sci_execution_state
  )
  EXECUTE FUNCTION group_standing_observation_id_mark();

-- A visitor observation completes where time accounting ends its charge
-- (c_ta_visitor_end), and the time accounting sweep moves that end after the
-- observation has completed at its slew.  So a change to it marks the tree
-- holding the observation.  Only visitor visits have a charge end, so time
-- accounting's updates of other visits mark nothing.
CREATE TRIGGER group_standing_visitor_end_mark_trigger
  AFTER UPDATE OF c_ta_visitor_end ON t_visit
  FOR EACH ROW
  WHEN (OLD.c_ta_visitor_end IS DISTINCT FROM NEW.c_ta_visitor_end)
  EXECUTE FUNCTION group_standing_observation_id_mark();

-- The calculation state of a tree, given its standing row (NULL when never
-- calculated): pending when never calculated, retry after a failure until it
-- is calculated again (edits wait for the retry time, so marks don't change
-- that), pending while marked or once time passing may have changed it, ready
-- otherwise.  A tree whose first calculation fails has placeholder rows, so
-- that it reads retry rather than never calculated.
CREATE FUNCTION group_standing_calc_state(
  root          d_group_id,
  retry_at      timestamp,
  reevaluate_at timestamp
) RETURNS e_calculation_state
  LANGUAGE sql STABLE AS $$
    SELECT CASE
      WHEN root IS NULL THEN 'pending'
      WHEN retry_at IS NOT NULL THEN 'retry'
      WHEN EXISTS (SELECT 1 FROM t_group_standing_mark m WHERE m.c_root_group_id = root) THEN 'pending'
      WHEN reevaluate_at <= now() THEN 'pending'
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
    group_standing_calc_state(s.c_root_group_id, c.c_retry_at, c.c_reevaluate_at) AS c_calc_state
  FROM t_group g
  LEFT JOIN t_group_standing      s ON s.c_group_id = g.c_group_id
  LEFT JOIN t_group_standing_calc c ON c.c_group_id = s.c_root_group_id
  WHERE g.c_existence = 'present';

-- The standing of each observation: its group's, or viable (and nothing to
-- calculate) outside any group.  An observation in a deleted group, itself
-- deleted, stands as one outside any group: its group has no standing, and
-- nothing will calculate one.
CREATE VIEW v_observation_group_standing AS
  SELECT
    o.c_observation_id,
    COALESCE(s.c_standing, 'viable' :: e_group_standing) AS c_standing,
    s.c_deciding_group_id,
    CASE
      WHEN g.c_group_id IS NULL THEN 'ready' :: e_calculation_state
      ELSE group_standing_calc_state(s.c_root_group_id, c.c_retry_at, c.c_reevaluate_at)
    END AS c_calc_state
  FROM t_observation o
  LEFT JOIN t_group               g ON g.c_group_id = o.c_group_id AND g.c_existence = 'present'
  LEFT JOIN t_group_standing      s ON s.c_group_id = g.c_group_id
  LEFT JOIN t_group_standing_calc c ON c.c_group_id = s.c_root_group_id;

-- Calculate every existing tree.
INSERT INTO t_group_standing_mark (c_root_group_id)
  SELECT c_group_id FROM t_group WHERE c_parent_id IS NULL AND c_existence = 'present';
