-- Rename columns to match long slit
ALTER TABLE t_gmos_north_imaging RENAME COLUMN c_explicit_bin TO c_bin;
ALTER TABLE t_gmos_north_imaging RENAME COLUMN c_explicit_amp_read_mode TO c_amp_read_mode;
ALTER TABLE t_gmos_north_imaging RENAME COLUMN c_explicit_amp_gain TO c_amp_gain;
ALTER TABLE t_gmos_north_imaging RENAME COLUMN c_explicit_roi TO c_roi;

-- Apply same changes to south imaging table
ALTER TABLE t_gmos_south_imaging RENAME COLUMN c_explicit_bin TO c_bin;
ALTER TABLE t_gmos_south_imaging RENAME COLUMN c_explicit_amp_read_mode TO c_amp_read_mode;
ALTER TABLE t_gmos_south_imaging RENAME COLUMN c_explicit_amp_gain TO c_amp_gain;
ALTER TABLE t_gmos_south_imaging RENAME COLUMN c_explicit_roi TO c_roi;

-- Add spatial offsets to GMOS imaging mode tables
--
-- c_explicit_spatial_offsets column stores spatial offset as text
-- The format is a comma-separated list of decimal values representing p and q in arcsecs
-- e.g.: "p1,q1,p2,q2,p3,q3,..."

ALTER TABLE t_gmos_north_imaging
ADD COLUMN c_spatial_offsets text NULL;

ALTER TABLE t_gmos_south_imaging
ADD COLUMN c_spatial_offsets text NULL;

ALTER TABLE t_gmos_north_imaging
ADD CONSTRAINT north_imaging_spatial_offsets_format CHECK (c_spatial_offsets IS NULL OR c_spatial_offsets ~ '^-?\d+(\.\d+)?(,-?\d+(\.\d+)?)*$');

ALTER TABLE t_gmos_south_imaging
ADD CONSTRAINT south_imaging_spatial_offsets_format CHECK (c_spatial_offsets IS NULL OR c_spatial_offsets ~ '^-?\d+(\.\d+)?(,-?\d+(\.\d+)?)*$');

DROP VIEW IF EXISTS v_gmos_north_imaging;
CREATE VIEW v_gmos_north_imaging AS
  SELECT
    i.*,
    f.c_filters
  FROM
    t_gmos_north_imaging i
  LEFT JOIN v_gmos_north_imaging_filter f
    ON i.c_observation_id = f.c_observation_id;

DROP VIEW IF EXISTS v_gmos_south_imaging;
CREATE VIEW v_gmos_south_imaging AS
  SELECT
    i.*,
    f.c_filters
  FROM
    t_gmos_south_imaging i
  LEFT JOIN v_gmos_south_imaging_filter f
    ON i.c_observation_id = f.c_observation_id;
