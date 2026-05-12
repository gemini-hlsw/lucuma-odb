-- Add (optional) sky coordinates
ALTER TABLE t_ghost_ifu
  ADD COLUMN c_sky_ra  d_angle_µas NULL DEFAULT NULL,
  ADD COLUMN c_sky_dec d_angle_µas NULL DEFAULT NULL,
  ADD CONSTRAINT ghost_ifu_sky_position_neither_or_both CHECK (
    (c_sky_ra IS NULL) = (c_sky_dec IS NULL)
  );

DROP VIEW v_ghost_ifu;

CREATE VIEW v_ghost_ifu AS
SELECT
  g.*,
  CASE WHEN c_slit_viewing_camera_exposure_time IS NOT NULL THEN c_observation_id END AS c_slit_viewing_camera_id,
  CASE WHEN c_sky_ra IS NOT NULL THEN c_observation_id END AS c_sky_id
FROM t_ghost_ifu g;
