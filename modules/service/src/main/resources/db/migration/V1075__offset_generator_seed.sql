ALTER TABLE t_offset_generator
  ADD COLUMN c_seed bigint NOT NULL DEFAULT floor(random() * 9223372036854775807)::bigint;

DROP VIEW v_gmos_north_imaging;

ALTER TABLE t_gmos_north_imaging
  DROP COLUMN c_sky_seed;

ALTER TABLE t_gmos_north_imaging_filter
  DROP COLUMN c_seed;

DROP VIEW v_gmos_south_imaging;

ALTER TABLE t_gmos_south_imaging
  DROP COLUMN c_sky_seed;

ALTER TABLE t_gmos_south_imaging_filter
  DROP COLUMN c_seed;

-- Copied from V1072__gmos_imaging2.sql
CREATE VIEW v_gmos_north_imaging AS
  SELECT
    i.*,
    f.c_filters,
    CASE WHEN i.c_variant = 'grouped'      THEN i.c_observation_id END AS c_grouped_observation_id,
    CASE WHEN i.c_variant = 'interleaved'  THEN i.c_observation_id END AS c_interleaved_observation_id,
    CASE WHEN i.c_variant = 'pre_imaging'  THEN i.c_observation_id END AS c_pre_imaging_observation_id
  FROM
    t_gmos_north_imaging i
  LEFT JOIN (
    SELECT
      c_observation_id,
      array_remove(array_agg(c_filter ORDER BY c_filter), NULL) AS c_filters
    FROM t_gmos_north_imaging_filter
    WHERE c_version = 'current'
    GROUP BY c_observation_id
  ) AS f USING (c_observation_id);

CREATE VIEW v_gmos_south_imaging AS
  SELECT
    i.*,
    f.c_filters,
    CASE WHEN i.c_variant = 'grouped'      THEN i.c_observation_id END AS c_grouped_observation_id,
    CASE WHEN i.c_variant = 'interleaved'  THEN i.c_observation_id END AS c_interleaved_observation_id,
    CASE WHEN i.c_variant = 'pre_imaging'  THEN i.c_observation_id END AS c_pre_imaging_observation_id
  FROM
    t_gmos_south_imaging i
  LEFT JOIN (
    SELECT
      c_observation_id,
      array_remove(array_agg(c_filter ORDER BY c_filter), NULL) AS c_filters
    FROM t_gmos_south_imaging_filter
    WHERE c_version = 'current'
    GROUP BY c_observation_id
  ) AS f USING (c_observation_id);