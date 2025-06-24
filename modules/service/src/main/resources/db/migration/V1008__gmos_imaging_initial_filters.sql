-- Add support for initial filters in GMOS Imaging modes

CREATE TABLE t_gmos_north_imaging_initial_filter (
  c_observation_id d_observation_id NOT NULL,
  c_filter         d_tag            NOT NULL REFERENCES t_gmos_north_filter(c_tag),
  PRIMARY KEY (c_observation_id, c_filter),
  FOREIGN KEY (c_observation_id) REFERENCES t_gmos_north_imaging(c_observation_id) ON DELETE CASCADE
);

CREATE TABLE t_gmos_south_imaging_initial_filter (
  c_observation_id d_observation_id NOT NULL,
  c_filter         d_tag            NOT NULL REFERENCES t_gmos_south_filter(c_tag),
  PRIMARY KEY (c_observation_id, c_filter),
  FOREIGN KEY (c_observation_id) REFERENCES t_gmos_south_imaging(c_observation_id) ON DELETE CASCADE
);

CREATE VIEW v_gmos_north_imaging_initial_filter AS
  SELECT
    c_observation_id,
    array_remove(array_agg(c_filter ORDER BY c_filter), NULL) AS c_initial_filters
  FROM
    t_gmos_north_imaging_initial_filter
  GROUP BY
    c_observation_id;

CREATE VIEW v_gmos_south_imaging_initial_filter AS
  SELECT
    c_observation_id,
    array_remove(array_agg(c_filter ORDER BY c_filter), NULL) AS c_initial_filters
  FROM
    t_gmos_south_imaging_initial_filter
  GROUP BY
    c_observation_id;

DROP VIEW v_gmos_north_imaging;

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

DROP VIEW v_gmos_south_imaging;

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
