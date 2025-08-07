-- Simplify GMOS imaging spatial offsets
ALTER TABLE t_gmos_north_imaging RENAME COLUMN c_spatial_offsets TO c_offsets;
ALTER TABLE t_gmos_south_imaging RENAME COLUMN c_spatial_offsets TO c_offsets;

UPDATE t_gmos_north_imaging SET c_offsets = '' WHERE c_offsets IS NULL;
UPDATE t_gmos_south_imaging SET c_offsets = '' WHERE c_offsets IS NULL;

ALTER TABLE t_gmos_north_imaging ALTER COLUMN c_offsets SET NOT NULL;
ALTER TABLE t_gmos_south_imaging ALTER COLUMN c_offsets SET NOT NULL;

ALTER TABLE t_gmos_north_imaging DROP CONSTRAINT north_imaging_spatial_offsets_format;
ALTER TABLE t_gmos_north_imaging ADD CONSTRAINT north_imaging_offsets_format CHECK (c_offsets = '' OR c_offsets ~ '^-?\d+(\.\d+)?(,-?\d+(\.\d+)?)*$');

ALTER TABLE t_gmos_south_imaging DROP CONSTRAINT south_imaging_spatial_offsets_format;
ALTER TABLE t_gmos_south_imaging ADD CONSTRAINT south_imaging_offsets_format CHECK (c_offsets = '' OR c_offsets ~ '^-?\d+(\.\d+)?(,-?\d+(\.\d+)?)*$');

-- crecreate the views
DROP VIEW IF EXISTS v_gmos_north_imaging;
CREATE VIEW v_gmos_north_imaging AS
  SELECT
    i.*,
    f.c_filters,
    fs.c_initial_filters
  FROM
    t_gmos_north_imaging i
  INNER JOIN v_gmos_north_imaging_filter f
    ON i.c_observation_id = f.c_observation_id
  INNER JOIN v_gmos_north_imaging_initial_filter fs
    ON i.c_observation_id = fs.c_observation_id;

DROP VIEW IF EXISTS v_gmos_south_imaging;
CREATE VIEW v_gmos_south_imaging AS
  SELECT
    i.*,
    f.c_filters,
    fs.c_initial_filters
  FROM
    t_gmos_south_imaging i
  INNER JOIN v_gmos_south_imaging_filter f
    ON i.c_observation_id = f.c_observation_id
  INNER JOIN v_gmos_south_imaging_initial_filter fs
    ON i.c_observation_id = fs.c_observation_id;
