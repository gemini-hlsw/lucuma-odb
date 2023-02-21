-- A view of the asterism table that only shows one target per observation, chosen arbitrarily for
-- now but we need to have it select the appopriate target for default binning calculations.
CREATE VIEW v_asterism_single_target_for_long_slit AS
SELECT
   c_program_id,
   c_observation_id,
   min(c_target_id) c_target_id
FROM 
  t_asterism_target
GROUP BY c_program_id, c_observation_id;

-- And update these views to use the view above.

CREATE OR REPLACE VIEW v_gmos_north_long_slit AS
SELECT
  m.*,
  o.c_image_quality,
  t.c_source_profile
FROM
  t_gmos_north_long_slit m
INNER JOIN t_observation o
  ON m.c_observation_id = o.c_observation_id
LEFT JOIN v_asterism_single_target_for_long_slit a
  ON m.c_observation_id = a.c_observation_id
LEFT JOIN t_target t
  ON a.c_target_id      = t.c_target_id;

CREATE OR REPLACE VIEW v_gmos_south_long_slit AS
SELECT
  m.*,
  o.c_image_quality,
  t.c_source_profile
FROM
  t_gmos_south_long_slit m
INNER JOIN t_observation o
  ON m.c_observation_id = o.c_observation_id
LEFT JOIN v_asterism_single_target_for_long_slit a
  ON m.c_observation_id = a.c_observation_id
LEFT JOIN t_target t
  ON a.c_target_id      = t.c_target_id;

  