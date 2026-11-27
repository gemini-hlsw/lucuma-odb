-- Every group paired with itself and with each group below it, at any depth.
CREATE VIEW v_group_ancestor AS
WITH RECURSIVE a(c_ancestor_id, c_group_id) AS (
  SELECT c_group_id, c_group_id FROM t_group
  UNION ALL
  SELECT a.c_ancestor_id, g.c_group_id
  FROM   a
  JOIN   t_group g ON g.c_parent_id = a.c_group_id
)
SELECT c_ancestor_id, c_group_id FROM a;

-- Every group paired with each element under it, at any depth.
CREATE VIEW v_group_descendant_element AS
SELECT
  a.c_ancestor_id,
  a.c_ancestor_id || ':' || e.c_group_element_id AS c_group_element_id,
  e.c_program_id,
  e.c_group_id,
  e.c_index,
  e.c_child_group_id,
  e.c_child_observation_id,
  e.c_existence
FROM v_group_ancestor a
JOIN v_group_element  e ON e.c_group_id = a.c_group_id;

-- A deleted group may hold a present system group with no present science observation: the
-- Calibrations process removes it once its science observation is deleted.
CREATE OR REPLACE PROCEDURE group_verify_existence(pid d_program_id) AS $$
DECLARE
  eid text;
BEGIN
  SELECT e.c_group_element_id
  INTO   eid
  FROM   v_group_element e
  JOIN   t_group g ON e.c_group_id = g.c_group_id
  WHERE  g.c_existence = 'deleted'
  AND    e.c_existence = 'present'
  AND    g.c_program_id = pid
  AND NOT EXISTS (
    SELECT 1
    FROM   t_group c
    WHERE  c.c_group_id::text = e.c_child_group_id::text
    AND    c.c_system
    AND    NOT EXISTS (
      SELECT 1 FROM t_observation o
      WHERE  o.c_group_id = c.c_group_id
      AND    o.c_calibration_role IS NULL
      AND    o.c_existence = 'present'
    )
  );
  IF FOUND THEN
    RAISE EXCEPTION 'Deleted group contains non-deleted element(s)';
  END IF;
END;
$$ LANGUAGE plpgsql;
