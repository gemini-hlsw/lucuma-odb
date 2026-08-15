-- Base position per observation at J2000 , for the observations
-- targetCoordinates cone filter.
-- the explicit base if set, otherwise the asterism composite with every target
-- proper-motion corrected to epoch J2000.
-- Null when the observation contains a non-sidereal or opportunity
-- target (and no explicit base), or when obscalc has not yet computed it.
-- Written by the obscalc worker alongside its other results.
ALTER TABLE t_obscalc
  ADD COLUMN c_j2000_base_ra  d_angle_µas NULL,
  ADD COLUMN c_j2000_base_dec d_angle_µas NULL;

-- B-tree indexes to support fast cone (angular-distance) searches via
-- bounding-box mirroring the configuration-request indexes in V1250.
CREATE INDEX IF NOT EXISTS ix_obscalc_j2000_base_ra
  ON t_obscalc (c_j2000_base_ra);

CREATE INDEX IF NOT EXISTS ix_obscalc_j2000_base_dec
  ON t_obscalc (c_j2000_base_dec);

-- Backfill: settled entries never recompute on their own, so mark them
-- pending and let the worker fill the new columns as it churns.  ITC lookups
-- are cached by input hash, so the sweep mostly recomputes digests.
UPDATE t_obscalc SET
  c_last_invalidation = NOW(),
  c_failure_count     = 0,
  c_retry_at          = NULL,
  c_obscalc_state     = 'pending'
WHERE c_obscalc_state IN ('ready', 'retry');
