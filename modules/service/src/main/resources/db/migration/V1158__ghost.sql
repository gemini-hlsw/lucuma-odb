DROP VIEW v_ghost_static;

ALTER TABLE t_ghost_static
  DROP COLUMN c_visit_id,
  ADD COLUMN  c_ifu_mapping    e_ghost_ifu_mapping NOT NULL,

  ADD COLUMN  c_ifu1_ra        d_angle_µas         NULL,
  ADD COLUMN  c_ifu1_dec       d_angle_µas         NULL,
  ADD COLUMN  c_ifu1_epoch     d_epoch             NULL,
  ADD COLUMN  c_ifu1_pm_ra     d_angle_µas         NULL,
  ADD COLUMN  c_ifu1_pm_dec    d_angle_µas         NULL,
  ADD COLUMN  c_ifu1_rv        numeric             NULL,
  ADD COLUMN  c_ifu1_parallax  d_angle_µas         NULL,

  ADD COLUMN  c_ifu2_ra        d_angle_µas         NULL,
  ADD COLUMN  c_ifu2_dec       d_angle_µas         NULL,
  ADD COLUMN  c_ifu2_epoch     d_epoch             NULL,
  ADD COLUMN  c_ifu2_pm_ra     d_angle_µas         NULL,
  ADD COLUMN  c_ifu2_pm_dec    d_angle_µas         NULL,
  ADD COLUMN  c_ifu2_rv        numeric             NULL,
  ADD COLUMN  c_ifu2_parallax  d_angle_µas         NULL,

  ADD CONSTRAINT t_ghost_static_consistent_ifu_mapping CHECK (
    CASE
      WHEN c_ifu_mapping = 'nonsidereal'     THEN
        num_nulls(
          c_ifu1_ra, c_ifu1_dec, c_ifu1_epoch, c_ifu1_pm_ra, c_ifu1_pm_dec, c_ifu1_rv, c_ifu1_parallax,
          c_ifu2_ra, c_ifu2_dec, c_ifu2_epoch, c_ifu2_pm_ra, c_ifu2_pm_dec, c_ifu2_rv, c_ifu2_parallax
        ) = 14

      WHEN c_ifu_mapping = 'single_target'   THEN
        num_nulls(c_ifu1_ra, c_ifu1_dec, c_ifu1_epoch) = 0 AND
        num_nulls(
          c_ifu2_ra, c_ifu2_dec, c_ifu2_epoch, c_ifu2_pm_ra, c_ifu2_pm_dec, c_ifu2_rv, c_ifu2_parallax
        ) = 7

      WHEN c_ifu_mapping = 'target_plus_sky' THEN
        num_nulls(
          c_ifu1_ra, c_ifu1_dec, c_ifu1_epoch, c_ifu2_ra, c_ifu2_dec
        ) = 0 AND
        num_nulls(
          c_ifu2_epoch, c_ifu2_pm_ra, c_ifu2_pm_dec, c_ifu2_rv, c_ifu2_parallax
        ) = 5

      WHEN c_ifu_mapping = 'sky_plus_target' THEN
        num_nulls(
          c_ifu2_ra, c_ifu2_dec, c_ifu2_epoch, c_ifu1_ra, c_ifu1_dec
        ) = 0 AND
        num_nulls(
          c_ifu1_epoch, c_ifu1_pm_ra, c_ifu1_pm_dec, c_ifu1_rv, c_ifu1_parallax
        ) = 5

      WHEN c_ifu_mapping = 'dual_target'     THEN
        num_nulls(c_ifu1_ra, c_ifu1_dec, c_ifu1_epoch) = 0 AND
        num_nulls(c_ifu2_ra, c_ifu2_dec, c_ifu2_epoch) = 0

      ELSE FALSE
    END
  );

CREATE VIEW v_ghost_static AS
SELECT
  g.*,
  CASE WHEN c_slit_viewing_camera_exposure_time IS NOT NULL THEN c_static_id END AS c_slit_viewing_camera_id
FROM t_ghost_static g;