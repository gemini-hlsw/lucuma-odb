-- Migration: Replace IFU coordinate/proper-motion columns with target_id
-- foreign keys; remove 'nonsidereal' from e_ghost_ifu_mapping.
--
-- Precondition: t_ghost_static has no rows (development environment).

DROP VIEW v_ghost_static;

-- -----------------------------------------------------------------------
-- 1. Drop the old CHECK constraint before altering columns
-- -----------------------------------------------------------------------
ALTER TABLE t_ghost_static
  DROP CONSTRAINT t_ghost_static_consistent_ifu_mapping;

-- -----------------------------------------------------------------------
-- 2. Add a program id FK reference so that we can link to t_asterism_table.
-- -----------------------------------------------------------------------
ALTER TABLE t_ghost_static
  ADD COLUMN c_program_id d_program_id NOT NULL REFERENCES t_program(c_program_id);

-- -----------------------------------------------------------------------
-- 3. Replace the IFU sidereal columns with target_id references.
--    c_ifu1_ra/dec and c_ifu2_ra/dec are retained for sky positions.
-- -----------------------------------------------------------------------
ALTER TABLE t_ghost_static
  DROP COLUMN c_ifu1_epoch,
  DROP COLUMN c_ifu1_pm_ra,
  DROP COLUMN c_ifu1_pm_dec,
  DROP COLUMN c_ifu1_rv,
  DROP COLUMN c_ifu1_parallax,

  DROP COLUMN c_ifu2_epoch,
  DROP COLUMN c_ifu2_pm_ra,
  DROP COLUMN c_ifu2_pm_dec,
  DROP COLUMN c_ifu2_rv,
  DROP COLUMN c_ifu2_parallax,

  ADD COLUMN c_ifu1_target_id d_target_id NULL,
  ADD COLUMN c_ifu2_target_id d_target_id NULL;

-----------------------------------------------------------------------
-- 3. FK constraints: target_id must be a member of the observation's
--    asterism, enforced via t_asterism_target (cprogram_id, cobservation_id,
--    c_target_id).
-- -----------------------------------------------------------------------
ALTER TABLE t_ghost_static
  ADD CONSTRAINT t_ghost_static_ifu1_target_fk
    FOREIGN KEY (c_program_id, c_observation_id, c_ifu1_target_id)
    REFERENCES t_asterism_target (c_program_id, c_observation_id, c_target_id),

  ADD CONSTRAINT t_ghost_static_ifu2_target_fk
    FOREIGN KEY (c_program_id, c_observation_id, c_ifu2_target_id)
    REFERENCES t_asterism_target (c_program_id, c_observation_id, c_target_id);

-- -----------------------------------------------------------------------
-- 4. Remove 'nonsidereal' from e_ghost_ifu_mapping.
--    PostgreSQL has no DROP VALUE so we rename, recreate, cast, clean up.
-- -----------------------------------------------------------------------
ALTER TYPE e_ghost_ifu_mapping RENAME TO e_ghost_ifu_mapping_old;

CREATE TYPE e_ghost_ifu_mapping AS ENUM (
  'single_target',
  'target_plus_sky',
  'sky_plus_target',
  'dual_target'
);

ALTER TABLE t_ghost_static
  ALTER COLUMN c_ifu_mapping TYPE e_ghost_ifu_mapping
    USING c_ifu_mapping::text::e_ghost_ifu_mapping;

DROP TYPE e_ghost_ifu_mapping_old;

-- -----------------------------------------------------------------------
-- 5. Add the revised CHECK constraint
-- -----------------------------------------------------------------------
ALTER TABLE t_ghost_static
  ADD CONSTRAINT t_ghost_static_consistent_ifu_mapping CHECK (
    CASE
      -- Single target (including nonsidereal targets): IFU1 points to a
      -- target; all IFU2 columns are null.
      WHEN c_ifu_mapping = 'single_target'   THEN
        c_ifu1_target_id IS NOT NULL AND
        num_nulls(c_ifu1_ra, c_ifu1_dec, c_ifu2_target_id, c_ifu2_ra, c_ifu2_dec) = 5

      -- IFU1 on target, IFU2 on sky position (ra/dec only, no target row).
      WHEN c_ifu_mapping = 'target_plus_sky' THEN
        num_nulls(c_ifu1_target_id, c_ifu2_ra, c_ifu2_dec) = 0 AND
        num_nulls(c_ifu1_ra, c_ifu1_dec, c_ifu2_target_id) = 3

      -- IFU1 on sky position (ra/dec only), IFU2 on target.
      WHEN c_ifu_mapping = 'sky_plus_target' THEN
        num_nulls(c_ifu1_ra, c_ifu1_dec, c_ifu2_target_id) = 0 AND
        num_nulls(c_ifu1_target_id, c_ifu2_ra, c_ifu2_dec) = 3

      -- Both IFUs on distinct targets; no sky ra/dec columns used.
      WHEN c_ifu_mapping = 'dual_target'     THEN
        num_nulls(c_ifu1_target_id, c_ifu2_target_id)           = 0 AND
        num_nulls(c_ifu1_ra, c_ifu1_dec, c_ifu2_ra, c_ifu2_dec) = 4

      ELSE FALSE
    END
  );

-- -----------------------------------------------------------------------
-- 6. Recreate the view (unchanged)
-- -----------------------------------------------------------------------
CREATE VIEW v_ghost_static AS
SELECT
  g.*,
  CASE WHEN c_slit_viewing_camera_exposure_time IS NOT NULL THEN c_static_id END AS c_slit_viewing_camera_id
FROM t_ghost_static g;