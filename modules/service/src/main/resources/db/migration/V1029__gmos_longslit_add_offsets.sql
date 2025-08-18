-- Rename spatial_offsets columns to offsets for GMOS long slit
ALTER TABLE t_gmos_north_long_slit RENAME COLUMN c_spatial_offsets TO c_offsets;
ALTER TABLE t_gmos_south_long_slit RENAME COLUMN c_spatial_offsets TO c_offsets;

ALTER TABLE t_gmos_north_long_slit RENAME CONSTRAINT offset_format TO gmos_north_longslit_offsets_format;
ALTER TABLE t_gmos_south_long_slit RENAME CONSTRAINT offset_format TO gmos_south_longslit_offsets_format;

-- remove unused the views
DROP VIEW IF EXISTS v_gmos_north_long_slit;
DROP VIEW IF EXISTS v_gmos_south_long_slit;
