-- Remove the instrument and observing mode type values from observations
-- without corresponding observing mode table entries.

UPDATE t_observation o
SET c_instrument          = NULL,
    c_observing_mode_type = NULL
WHERE
  o.c_observing_mode_type = 'gmos_north_long_slit' AND
  NOT EXISTS (
    SELECT 1 FROM t_gmos_north_long_slit m WHERE m.c_observation_id = o.c_observation_id
  );

UPDATE t_observation o
SET c_instrument          = NULL,
    c_observing_mode_type = NULL
WHERE
  o.c_observing_mode_type = 'gmos_south_long_slit' AND
  NOT EXISTS (
    SELECT 1 FROM t_gmos_south_long_slit m WHERE m.c_observation_id = o.c_observation_id
  );

UPDATE t_observation o
SET c_instrument          = NULL,
    c_observing_mode_type = NULL
WHERE
  o.c_observing_mode_type = 'gmos_north_imaging' AND
  NOT EXISTS (
    SELECT 1 FROM t_gmos_north_imaging m WHERE m.c_observation_id = o.c_observation_id
  );

UPDATE t_observation o
SET c_instrument          = NULL,
    c_observing_mode_type = NULL
WHERE
  o.c_observing_mode_type = 'gmos_south_imaging' AND
  NOT EXISTS (
    SELECT 1 FROM t_gmos_south_imaging m WHERE m.c_observation_id = o.c_observation_id
  );

UPDATE t_observation o
SET c_instrument          = NULL,
    c_observing_mode_type = NULL
WHERE
  o.c_observing_mode_type = 'flamingos_2_long_slit' AND
  NOT EXISTS (
    SELECT 1 FROM t_flamingos_2_long_slit m WHERE m.c_observation_id = o.c_observation_id
  );