-- Number of consecutive science exposures to collect at each object offset
-- position in the "grouped" imaging variant.  Shared shape with the other
-- imaging mode tables (gmos north/south, gnirs, flamingos2); only GMOS exposes
-- it through the API for now, so the other modes simply carry the default of 1.

ALTER TABLE t_gmos_north_imaging
  ADD COLUMN c_exposures_per_offset int NOT NULL DEFAULT 1 CHECK (c_exposures_per_offset > 0);

ALTER TABLE t_gmos_south_imaging
  ADD COLUMN c_exposures_per_offset int NOT NULL DEFAULT 1 CHECK (c_exposures_per_offset > 0);

ALTER TABLE t_gnirs_imaging
  ADD COLUMN c_exposures_per_offset int NOT NULL DEFAULT 1 CHECK (c_exposures_per_offset > 0);

ALTER TABLE t_flamingos_2_imaging
  ADD COLUMN c_exposures_per_offset int NOT NULL DEFAULT 1 CHECK (c_exposures_per_offset > 0);

-- The imaging views SELECT i.*, which freezes the column list at creation time,
-- so they must be recreated to pick up the new column.

DROP VIEW v_gmos_north_imaging;

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

DROP VIEW v_gmos_south_imaging;

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

DROP VIEW v_gnirs_imaging;

CREATE VIEW v_gnirs_imaging AS
  SELECT
    i.*,
    -- well depth default: mirrors GnirsWellDepth.forCamera
    -- ATTENTION: This logic is duplicated from lucuma-core GnirsWellDepth. Modify in sync.
    (CASE
      WHEN i.c_camera IN ('ShortBlue', 'LongBlue') THEN 'Shallow'
      WHEN i.c_camera IN ('ShortRed',  'LongRed')  THEN 'Deep'
    END)::e_gnirs_well_depth AS c_well_depth_default,
    f.c_filters,
    CASE WHEN i.c_variant = 'grouped'      THEN i.c_observation_id END AS c_grouped_observation_id,
    CASE WHEN i.c_variant = 'interleaved'  THEN i.c_observation_id END AS c_interleaved_observation_id,
    CASE WHEN i.c_variant = 'pre_imaging'  THEN i.c_observation_id END AS c_pre_imaging_observation_id
  FROM
    t_gnirs_imaging i
  LEFT JOIN (
    SELECT
      c_observation_id,
      array_remove(array_agg(c_filter ORDER BY c_filter), NULL) AS c_filters
    FROM t_gnirs_imaging_filter
    WHERE c_version = 'current'
    GROUP BY c_observation_id
  ) AS f USING (c_observation_id);

DROP VIEW v_flamingos_2_imaging;

CREATE VIEW v_flamingos_2_imaging AS
  SELECT
    i.*,
    f.c_filters,
    CASE WHEN i.c_variant = 'grouped'      THEN i.c_observation_id END AS c_grouped_observation_id,
    CASE WHEN i.c_variant = 'interleaved'  THEN i.c_observation_id END AS c_interleaved_observation_id,
    CASE WHEN i.c_variant = 'pre_imaging'  THEN i.c_observation_id END AS c_pre_imaging_observation_id
  FROM
    t_flamingos_2_imaging i
  LEFT JOIN (
    SELECT
      c_observation_id,
      array_remove(array_agg(c_filter ORDER BY c_filter), NULL) AS c_filters
    FROM t_flamingos_2_imaging_filter
    WHERE c_version = 'current'
    GROUP BY c_observation_id
  ) AS f USING (c_observation_id);
