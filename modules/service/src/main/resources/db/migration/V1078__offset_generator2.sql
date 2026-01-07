DROP VIEW v_offset_generator;

-- We need a view with synthetic ids for grackle.
CREATE VIEW v_offset_generator AS
  SELECT o.*,
  CASE WHEN o.c_role = 'object'     THEN o.c_observation_id END AS c_object_observation_id,
  CASE WHEN o.c_role = 'sky'        THEN o.c_observation_id END AS c_sky_observation_id,
  CASE WHEN o.c_type = 'enumerated' THEN o.c_observation_id END AS c_enumerated_observation_id,
  CASE WHEN o.c_type = 'enumerated' THEN o.c_role           END AS c_enumerated_role,
  CASE WHEN o.c_type = 'random'     THEN o.c_observation_id END AS c_random_observation_id,
  CASE WHEN o.c_type = 'random'     THEN o.c_role           END AS c_random_role,
  CASE WHEN o.c_type = 'random'     THEN o.c_seed           END AS c_random_seed,
  CASE WHEN o.c_type = 'spiral'     THEN o.c_observation_id END AS c_spiral_observation_id,
  CASE WHEN o.c_type = 'spiral'     THEN o.c_role           END AS c_spiral_role,
  CASE WHEN o.c_type = 'spiral'     THEN o.c_seed           END AS c_spiral_seed,
  CASE WHEN o.c_type = 'uniform'    THEN o.c_observation_id END AS c_uniform_observation_id,
  CASE WHEN o.c_type = 'uniform'    THEN o.c_role           END AS c_uniform_role
  FROM t_offset_generator o;