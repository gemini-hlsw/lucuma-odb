
-- Mode views that combine the mode fields with data from other tables required
-- to generate the corresponding sequence.

CREATE VIEW v_gmos_north_long_slit AS
SELECT
  m.*,
  o.c_image_quality,
  t.c_source_profile
FROM
  t_gmos_north_long_slit m
INNER JOIN t_observation o
  ON m.c_observation_id = o.c_observation_id
LEFT JOIN t_asterism_target a
  ON m.c_observation_id = a.c_observation_id
LEFT JOIN t_target t
  ON a.c_target_id      = t.c_target_id;

CREATE VIEW v_gmos_south_long_slit AS
SELECT
  m.*,
  o.c_image_quality,
  t.c_source_profile
FROM
  t_gmos_south_long_slit m
INNER JOIN t_observation o
  ON m.c_observation_id = o.c_observation_id
LEFT JOIN t_asterism_target a
  ON m.c_observation_id = a.c_observation_id
LEFT JOIN t_target t
  ON a.c_target_id      = t.c_target_id;