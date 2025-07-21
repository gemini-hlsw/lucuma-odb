ALTER TABLE t_gmos_north_imaging
  ADD c_multiple_filters_mode e_multiple_filters_mode NULL;

ALTER TABLE t_gmos_south_imaging
  ADD c_multiple_filters_mode e_multiple_filters_mode NULL;

-- Recreate views to include the new column
DROP VIEW IF EXISTS v_gmos_north_imaging;
DROP VIEW IF EXISTS v_gmos_south_imaging;

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
