ALTER TABLE t_gmos_north_imaging
  ADD COLUMN c_imaging_type       e_gmos_imaging_type NOT NULL DEFAULT 'grouped',
  ADD COLUMN c_wavelength_order   e_wavelength_order  NOT NULL DEFAULT 'decreasing',
  ADD COLUMN c_sky_count          int                 NOT NULL DEFAULT 0 CHECK (c_sky_count >= 0),
  ADD COLUMN c_pre_imaging_off1_p d_angle_µas         NOT NULL DEFAULT 0,
  ADD COLUMN c_pre_imaging_off1_q d_angle_µas         NOT NULL DEFAULT 0,
  ADD COLUMN c_pre_imaging_off2_p d_angle_µas         NOT NULL DEFAULT 0,
  ADD COLUMN c_pre_imaging_off2_q d_angle_µas         NOT NULL DEFAULT 0,
  ADD COLUMN c_pre_imaging_off3_p d_angle_µas         NOT NULL DEFAULT 0,
  ADD COLUMN c_pre_imaging_off3_q d_angle_µas         NOT NULL DEFAULT 0,
  ADD COLUMN c_pre_imaging_off4_p d_angle_µas         NOT NULL DEFAULT 0,
  ADD COLUMN c_pre_imaging_off4_q d_angle_µas         NOT NULL DEFAULT 0;

ALTER TABLE t_gmos_south_imaging
  ADD COLUMN c_imaging_type       e_gmos_imaging_type NOT NULL DEFAULT 'grouped',
  ADD COLUMN c_wavelength_order   e_wavelength_order  NOT NULL DEFAULT 'decreasing',
  ADD COLUMN c_sky_count          int                 NOT NULL DEFAULT 0 CHECK (c_sky_count >= 0),
  ADD COLUMN c_pre_imaging_off1_p d_angle_µas         NOT NULL DEFAULT 0,
  ADD COLUMN c_pre_imaging_off1_q d_angle_µas         NOT NULL DEFAULT 0,
  ADD COLUMN c_pre_imaging_off2_p d_angle_µas         NOT NULL DEFAULT 0,
  ADD COLUMN c_pre_imaging_off2_q d_angle_µas         NOT NULL DEFAULT 0,
  ADD COLUMN c_pre_imaging_off3_p d_angle_µas         NOT NULL DEFAULT 0,
  ADD COLUMN c_pre_imaging_off3_q d_angle_µas         NOT NULL DEFAULT 0,
  ADD COLUMN c_pre_imaging_off4_p d_angle_µas         NOT NULL DEFAULT 0,
  ADD COLUMN c_pre_imaging_off4_q d_angle_µas         NOT NULL DEFAULT 0;

-- Update the views to pick up the new columns.  Add embedded ids for the
-- imaging type options.

-- Evolution from V1064__offset_generator.sql version.

DROP VIEW v_gmos_north_imaging;

ALTER TABLE t_gmos_north_imaging
  DROP COLUMN c_multiple_filters_mode,
  DROP COLUMN c_offsets;

CREATE VIEW v_gmos_north_imaging AS
  SELECT
    i.*,
    f.c_filters,
    CASE WHEN i.c_imaging_type = 'grouped'      THEN i.c_observation_id END AS c_grouped_observation_id,
    CASE WHEN i.c_imaging_type = 'pre_imaging'  THEN i.c_observation_id END AS c_pre_imaging_observation_id
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

ALTER TABLE t_gmos_south_imaging
  DROP COLUMN c_multiple_filters_mode,
  DROP COLUMN c_offsets;

CREATE VIEW v_gmos_south_imaging AS
  SELECT
    i.*,
    f.c_filters,
    CASE WHEN i.c_imaging_type = 'grouped'      THEN i.c_observation_id END AS c_grouped_observation_id,
    CASE WHEN i.c_imaging_type = 'pre_imaging'  THEN i.c_observation_id END AS c_pre_imaging_observation_id
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