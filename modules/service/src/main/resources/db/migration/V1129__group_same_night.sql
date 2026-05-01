-- Add "Same Night" option to groups
ALTER TABLE t_group
ADD COLUMN c_same_night bool NOT NULL DEFAULT false;

-- Same Night is mutually exclusive with c_max_interval and only valid for
-- AND groups (c_min_required IS NULL).
ALTER TABLE t_group
ADD CONSTRAINT group_same_night_check
CHECK (c_same_night = false OR (c_max_interval IS NULL AND c_min_required IS NULL));

DROP VIEW v_group;

CREATE VIEW v_group AS
  SELECT *,
  CASE WHEN c_min_interval IS NOT NULL THEN c_group_id END AS c_min_interval_id,
  CASE WHEN c_max_interval IS NOT NULL THEN c_group_id END AS c_max_interval_id
  FROM t_group;
