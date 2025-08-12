-- Rename spatial_offsets columns to offsets for GMOS long slit
ALTER TABLE t_gmos_north_long_slit RENAME COLUMN c_spatial_offsets TO c_offsets;
ALTER TABLE t_gmos_south_long_slit RENAME COLUMN c_spatial_offsets TO c_offsets;

ALTER TABLE t_gmos_north_long_slit RENAME CONSTRAINT offset_format TO gmos_north_longslit_offsets_format;
ALTER TABLE t_gmos_south_long_slit RENAME CONSTRAINT offset_format TO gmos_south_longslit_offsets_format;

-- recreate the views
DROP VIEW IF EXISTS v_gmos_north_long_slit;
CREATE OR REPLACE VIEW v_gmos_north_long_slit AS
SELECT
  m.*
FROM
  t_gmos_north_long_slit m
INNER JOIN t_observation o
  ON m.c_observation_id = o.c_observation_id;

DROP VIEW IF EXISTS v_gmos_south_long_slit;
CREATE OR REPLACE VIEW v_gmos_south_long_slit AS
SELECT
  m.*
FROM
  t_gmos_south_long_slit m
INNER JOIN t_observation o
  ON m.c_observation_id = o.c_observation_id;
