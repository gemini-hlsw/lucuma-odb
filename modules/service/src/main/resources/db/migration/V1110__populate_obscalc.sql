-- Populate the obscalc table, having truncated it in the previous migration.
-- This will cause obscalc to recalculate all the things for all the
-- observations.

INSERT INTO t_obscalc (
  c_program_id,
  c_observation_id
)
SELECT
  o.c_program_id,
  o.c_observation_id
FROM t_observation o
ON CONFLICT (c_program_id, c_observation_id) DO NOTHING;