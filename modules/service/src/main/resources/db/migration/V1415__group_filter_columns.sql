-- Columns for filtering groups.  Whether a group has any waiver, and any
-- active one, for filtering by the absence of waivers: a filter over a
-- group's waivers matches some waiver, and can't say that none does.  Unless
-- a query uses them, the planner drops these subqueries along with the
-- columns.  And the intervals again, as plain nullable values to compare,
-- apart from the columns that make up the embedded interval objects.
DROP VIEW v_group;

CREATE VIEW v_group AS
  SELECT *,
  CASE WHEN c_min_interval IS NOT NULL THEN c_group_id END AS c_min_interval_id,
  CASE WHEN c_max_interval IS NOT NULL THEN c_group_id END AS c_max_interval_id,
  c_min_interval AS c_min_interval_value,
  c_max_interval AS c_max_interval_value,
  EXISTS (
    SELECT 1 FROM t_group_constraint_waiver w WHERE w.c_group_id = g.c_group_id
  ) AS c_has_waiver,
  EXISTS (
    SELECT 1 FROM t_group_constraint_waiver w WHERE w.c_group_id = g.c_group_id AND w.c_revoked_at IS NULL
  ) AS c_has_active_waiver
  FROM t_group g;
