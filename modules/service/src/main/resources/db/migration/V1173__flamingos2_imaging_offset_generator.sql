-- Flamingos2 imaging offsets should match gmos
ALTER TABLE t_flamingos_2_imaging
  ADD COLUMN c_variant            e_imaging_variant  NOT NULL DEFAULT 'grouped',
  ADD COLUMN c_wavelength_order   e_wavelength_order NOT NULL DEFAULT 'increasing',
  ADD COLUMN c_sky_count          int                NOT NULL DEFAULT 0 CHECK (c_sky_count >= 0),
  ADD COLUMN c_pre_imaging_off1_p d_angle_µas        NOT NULL DEFAULT 0,
  ADD COLUMN c_pre_imaging_off1_q d_angle_µas        NOT NULL DEFAULT 0,
  ADD COLUMN c_pre_imaging_off2_p d_angle_µas        NOT NULL DEFAULT 0,
  ADD COLUMN c_pre_imaging_off2_q d_angle_µas        NOT NULL DEFAULT 0,
  ADD COLUMN c_pre_imaging_off3_p d_angle_µas        NOT NULL DEFAULT 0,
  ADD COLUMN c_pre_imaging_off3_q d_angle_µas        NOT NULL DEFAULT 0,
  ADD COLUMN c_pre_imaging_off4_p d_angle_µas        NOT NULL DEFAULT 0,
  ADD COLUMN c_pre_imaging_off4_q d_angle_µas        NOT NULL DEFAULT 0;

DROP VIEW v_flamingos_2_imaging;

-- Unless somebody used the api there are no observations with offsets
ALTER TABLE t_flamingos_2_imaging
  DROP COLUMN c_offsets;

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

-- Clean up offset generators when the imaging mode row is deleted.
CREATE TRIGGER offset_generation_cleanup_trigger
AFTER DELETE ON t_flamingos_2_imaging
FOR EACH ROW
EXECUTE FUNCTION offset_generation_cleanup();
