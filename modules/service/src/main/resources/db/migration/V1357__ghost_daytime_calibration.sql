-- GHOST daytime calibrations: the IFU mapping gains a target-less value for
-- calibrations without an asterism, and GHOST joins the daytime instruments.

DROP VIEW v_ghost_static;

ALTER TABLE t_ghost_static
  DROP CONSTRAINT t_ghost_static_consistent_ifu_mapping;

ALTER TYPE e_ghost_ifu_mapping RENAME TO e_ghost_ifu_mapping_old;

CREATE TYPE e_ghost_ifu_mapping AS ENUM (
  'single_target',
  'target_plus_sky',
  'sky_plus_target',
  'dual_target',
  'no_target'
);

ALTER TABLE t_ghost_static
  ALTER COLUMN c_ifu_mapping TYPE e_ghost_ifu_mapping
    USING c_ifu_mapping::text::e_ghost_ifu_mapping;

DROP TYPE e_ghost_ifu_mapping_old;

ALTER TABLE t_ghost_static
  ADD CONSTRAINT t_ghost_static_consistent_ifu_mapping CHECK (
    CASE
      WHEN c_ifu_mapping = 'single_target'   THEN
        c_ifu1_target_id IS NOT NULL AND
        num_nulls(c_ifu1_ra, c_ifu1_dec, c_ifu2_target_id, c_ifu2_ra, c_ifu2_dec) = 5

      WHEN c_ifu_mapping = 'target_plus_sky' THEN
        num_nulls(c_ifu1_target_id, c_ifu2_ra, c_ifu2_dec) = 0 AND
        num_nulls(c_ifu1_ra, c_ifu1_dec, c_ifu2_target_id) = 3

      WHEN c_ifu_mapping = 'sky_plus_target' THEN
        num_nulls(c_ifu1_ra, c_ifu1_dec, c_ifu2_target_id) = 0 AND
        num_nulls(c_ifu1_target_id, c_ifu2_ra, c_ifu2_dec) = 3

      WHEN c_ifu_mapping = 'dual_target'     THEN
        num_nulls(c_ifu1_target_id, c_ifu2_target_id)           = 0 AND
        num_nulls(c_ifu1_ra, c_ifu1_dec, c_ifu2_ra, c_ifu2_dec) = 4

      -- Daytime calibrations: no IFU is assigned.
      WHEN c_ifu_mapping = 'no_target'       THEN
        num_nulls(c_ifu1_target_id, c_ifu1_ra, c_ifu1_dec,
                  c_ifu2_target_id, c_ifu2_ra, c_ifu2_dec) = 6

      ELSE FALSE
    END
  );

CREATE VIEW v_ghost_static AS
SELECT
  g.*,
  CASE WHEN c_slit_viewing_camera_exposure_time IS NOT NULL THEN c_static_id END AS c_slit_viewing_camera_id
FROM t_ghost_static g;

-- Allow GHOST daytime calibrations.
ALTER TABLE t_daytime_calibration
  DROP CONSTRAINT t_daytime_calibration_c_instrument_check;

ALTER TABLE t_daytime_calibration
  ADD CONSTRAINT t_daytime_calibration_c_instrument_check
    CHECK (c_instrument IN ('Flamingos2', 'Ghost', 'GmosNorth', 'GmosSouth', 'Gnirs', 'Igrins2'));
