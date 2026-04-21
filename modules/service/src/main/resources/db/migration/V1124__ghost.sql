ALTER TABLE t_ghost_ifu
  ADD COLUMN c_step_count                        int4     NOT NULL DEFAULT 1 CHECK (c_step_count > 0),
  ADD COLUMN c_slit_viewing_camera_exposure_time interval NULL DEFAULT NULL;

CREATE VIEW v_ghost_ifu AS
SELECT
  g.*,
  CASE WHEN c_slit_viewing_camera_exposure_time IS NOT NULL THEN c_observation_id END AS c_slit_viewing_camera_id
FROM t_ghost_ifu g;

ALTER TABLE t_ghost_static
  ADD COLUMN c_slit_viewing_camera_exposure_time interval NULL DEFAULT NULL;

CREATE VIEW v_ghost_static AS
SELECT
  g.*,
  CASE WHEN c_slit_viewing_camera_exposure_time IS NOT NULL THEN c_static_id END AS c_slit_viewing_camera_id
FROM t_ghost_static g;