-- GMOS spectroscopy science steps whose wavelength coverage reaches past twice
-- the shortest wavelength reaching the grating now carry a dismissable
-- SECOND_ORDER_OVERLAP warning (sc-10811).  The affected steps are found in the
-- same pass over the sequence that computes its digest, so edited sequences are
-- covered too, and kept with the rest of the acquisition and science digests:
-- in the obscalc result and in the digest cache.
--
-- In t_obscalc the digest is read as a whole, all of its columns null or none,
-- so the new columns are null exactly when the rest of the digest is.  An
-- existing digest gets an empty placeholder, which the recalculation below
-- replaces wherever it matters.
ALTER TABLE t_obscalc
  ADD COLUMN c_acq_second_order_overlaps jsonb NULL,
  ADD COLUMN c_sci_second_order_overlaps jsonb NULL;

UPDATE t_obscalc
   SET c_acq_second_order_overlaps = '[]'::jsonb,
       c_sci_second_order_overlaps = '[]'::jsonb
 WHERE c_acq_execution_state IS NOT NULL;

ALTER TABLE t_execution_digest
  ADD COLUMN c_acq_second_order_overlaps jsonb NOT NULL DEFAULT '[]'::jsonb,
  ADD COLUMN c_sci_second_order_overlaps jsonb NOT NULL DEFAULT '[]'::jsonb;

-- A cached GMOS spectroscopy digest was computed without the check, so its
-- empty placeholder could hide an overlap.  Drop them to be recomputed.
DELETE FROM t_execution_digest d
 USING t_observation o
 WHERE o.c_observation_id = d.c_observation_id
   AND o.c_observing_mode_type IN (
         'gmos_north_long_slit',
         'gmos_south_long_slit',
         'gmos_north_mos',
         'gmos_south_mos',
         'gmos_north_ifu',
         'gmos_south_ifu'
       );

-- Recalculate the GMOS spectroscopy observations where the warning would
-- matter: those with a sequence that are not finished.  Completed and
-- inactive ones are skipped, as in V1340.
UPDATE t_obscalc c SET
  c_last_invalidation = NOW(),
  c_failure_count     = 0,
  c_retry_at          = NULL,
  c_obscalc_state     = 'pending'
FROM t_observation o
WHERE o.c_observation_id = c.c_observation_id
  AND o.c_observing_mode_type IN (
        'gmos_north_long_slit',
        'gmos_south_long_slit',
        'gmos_north_mos',
        'gmos_south_mos',
        'gmos_north_ifu',
        'gmos_south_ifu'
      )
  AND c.c_workflow_state IN ('unapproved', 'defined', 'ready', 'ongoing');
