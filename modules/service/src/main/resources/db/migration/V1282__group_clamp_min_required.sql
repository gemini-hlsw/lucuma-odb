-- An OR group's c_min_required can go stale when elements leave, leaving it asking for more
-- elements than the group has. Clamp it down whenever the membership changes.
--
-- Empty groups are deliberately left alone: c_min_required is the only thing marking a group as
-- an OR group, so zeroing or nulling it would silently turn an emptied OR group into an AND
-- group. A stale value on an empty group is harmless and is clamped as soon as elements arrive.

-- Group membership was unindexed, which the per-row trigger below cannot afford.
CREATE INDEX i_observation_group ON t_observation (c_group_id);
CREATE INDEX i_group_parent      ON t_group       (c_parent_id);

-- Number of present elements (observations plus child groups) in a group. This is the one
-- definition of "element count"; the API-level range check calls it too, so the check and the
-- clamp below can never disagree.
CREATE OR REPLACE FUNCTION group_element_count(gid d_group_id) RETURNS bigint AS $$
  SELECT count(*)
  FROM   v_group_element
  WHERE  c_group_id = gid
  AND    c_existence = 'present';
$$ LANGUAGE sql STABLE;

CREATE OR REPLACE PROCEDURE group_clamp_min_required(gid d_group_id) AS $$
DECLARE
  n bigint;
BEGIN
  IF gid IS NULL THEN
    RETURN;
  END IF;
  n := group_element_count(gid);
  IF n > 0 THEN
    UPDATE t_group
    SET    c_min_required = n::int2
    WHERE  c_group_id     = gid
    AND    c_min_required IS NOT NULL
    AND    c_min_required > n;
  END IF;
END;
$$ LANGUAGE plpgsql;

-- Clamp both the group an observation left and the one it joined.
CREATE OR REPLACE FUNCTION t_observation_clamp_min_required() RETURNS TRIGGER AS $$
BEGIN
  IF TG_OP <> 'INSERT' THEN
    CALL group_clamp_min_required(OLD.c_group_id);
  END IF;
  IF TG_OP <> 'DELETE' THEN
    CALL group_clamp_min_required(NEW.c_group_id);
  END IF;
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION t_group_clamp_min_required() RETURNS TRIGGER AS $$
BEGIN
  IF TG_OP <> 'INSERT' THEN
    CALL group_clamp_min_required(OLD.c_parent_id);
  END IF;
  IF TG_OP <> 'DELETE' THEN
    CALL group_clamp_min_required(NEW.c_parent_id);
  END IF;
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

-- Clamp the group itself when its own c_min_required is written. This closes a TOCTOU gap: the
-- API-level range check reads the element count and then writes c_min_required in a later
-- statement, so a concurrent membership change committing in between could leave the write stale.
-- Because both this clamp and the membership clamp above UPDATE the same t_group row, they
-- serialize on that row lock, and whichever transaction commits second re-clamps against the
-- fully committed count.
CREATE OR REPLACE FUNCTION t_group_clamp_own_min_required() RETURNS TRIGGER AS $$
BEGIN
  CALL group_clamp_min_required(NEW.c_group_id);
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

-- Deferred, like the other group triggers: a move parks the element outside the group before
-- landing it, and an immediate trigger would clamp against that intermediate state (which would
-- decrement c_min_required on a plain reorder within the group).
--
-- The membership triggers fire only on membership columns; the self-clamp trigger fires only on
-- c_min_required. The clamp UPDATE lowers c_min_required to the count only when it currently
-- exceeds it, so the self-clamp trigger's own UPDATE re-fires the trigger at most once more, where
-- the value already equals the count and the UPDATE matches no rows. No infinite recursion.
CREATE CONSTRAINT TRIGGER clamp_min_required_observations
AFTER INSERT OR DELETE OR UPDATE OF c_group_id, c_existence ON t_observation
DEFERRABLE
FOR EACH ROW
EXECUTE FUNCTION t_observation_clamp_min_required();

CREATE CONSTRAINT TRIGGER clamp_min_required_groups
AFTER INSERT OR DELETE OR UPDATE OF c_parent_id, c_existence ON t_group
DEFERRABLE
FOR EACH ROW
EXECUTE FUNCTION t_group_clamp_min_required();

CREATE CONSTRAINT TRIGGER clamp_own_min_required
AFTER UPDATE OF c_min_required ON t_group
DEFERRABLE
FOR EACH ROW
EXECUTE FUNCTION t_group_clamp_own_min_required();

-- Fix up any existing rows that are already out of range.
UPDATE t_group g
SET    c_min_required = e.n::int2
FROM   (SELECT c_group_id AS gid, group_element_count(c_group_id) AS n FROM t_group) e
WHERE  g.c_group_id     = e.gid
AND    g.c_min_required IS NOT NULL
AND    e.n              > 0
AND    g.c_min_required > e.n;

-- A positive c_min_required is now the lower-bound invariant (see story sc-10033): an emptied OR
-- group keeps its old value and an AND group is null, so nothing needs a non-positive value to
-- mean anything. Fix up any existing rows, then enforce it on every write path with a constraint
-- so no direct or internal write can reintroduce 0 (or a negative).
UPDATE t_group
SET    c_min_required = 1
WHERE  c_min_required <= 0;

ALTER TABLE t_group
ADD CONSTRAINT group_min_required_positive
CHECK (c_min_required IS NULL OR c_min_required > 0);
