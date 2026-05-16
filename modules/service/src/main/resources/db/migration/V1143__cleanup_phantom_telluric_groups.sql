-- sc-8661: hide "phantom" system telluric groups whose parent science
-- observatiann was moved out and/or deleted.
--
-- We soft-delete rather than DELETE to avoid having to update all references.
-- Dev also has some obs with visits, we'll leave the phantom groups in that case.

-- 1) Find phantom telluric groups whose tellurics can be safely hidden.
CREATE TEMP TABLE tmp_phantom_telluric_groups AS
SELECT g.c_group_id
FROM   t_group g
WHERE  g.c_system    = true                       -- system-managed group
  AND  g.c_existence = 'present'                  -- not already hidden
  AND  'telluric'    = ANY(g.c_calibration_roles) -- telluric group
  AND  NOT EXISTS (                               -- no science obs (the buggy groups)
    SELECT 1
    FROM   t_observation s
    WHERE  s.c_group_id          = g.c_group_id
      AND  s.c_calibration_role IS NULL
      AND  s.c_existence         = 'present'
  )
  AND  NOT EXISTS (                               -- and no telluric inside has visits or events
    SELECT 1
    FROM   t_observation t
    WHERE  t.c_group_id = g.c_group_id
      AND  ( EXISTS (SELECT 1 FROM t_visit           v WHERE v.c_observation_id = t.c_observation_id)
          OR EXISTS (SELECT 1 FROM t_execution_event e WHERE e.c_observation_id = t.c_observation_id))
  );

-- 2) Soft delete the tellurics
UPDATE t_observation
SET    c_existence = 'deleted'
WHERE  c_group_id IN (SELECT c_group_id FROM tmp_phantom_telluric_groups)
  AND  c_existence = 'present';

-- 3) Soft delte the groups
UPDATE t_group
SET    c_existence = 'deleted'
WHERE  c_group_id IN (SELECT c_group_id FROM tmp_phantom_telluric_groups);

DROP TABLE tmp_phantom_telluric_groups;
