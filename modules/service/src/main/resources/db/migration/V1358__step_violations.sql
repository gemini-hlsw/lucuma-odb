-- The problems found in the steps of a sequence are now kept generally, as
-- step violations: each a validation code and a message (sc-10811).  The
-- exposure time violations become step violations, errors with code
-- CONFIGURATION_ERROR and warnings with code EXPOSURE_TIME_WARNING (the codes
-- they were always reported with), and GMOS spectroscopy steps exposed to second
-- order light add SECOND_ORDER_OVERLAP.
CREATE FUNCTION exposure_time_to_step_violations(violations jsonb)
  RETURNS jsonb AS $$
  SELECT CASE
    WHEN violations IS NULL THEN NULL
    ELSE COALESCE(
      (
        SELECT jsonb_agg(
                 jsonb_build_object(
                   'code',    CASE v->>'severity' WHEN 'ERROR' THEN 'CONFIGURATION_ERROR' ELSE 'EXPOSURE_TIME_WARNING' END,
                   'message', v->'description'
                 )
                 ORDER BY n
               )
          FROM jsonb_array_elements(violations) WITH ORDINALITY AS a(v, n)
      ),
      '[]'::jsonb
    )
  END
$$ LANGUAGE sql IMMUTABLE;

-- The obscalc results keep their violations, converted in place, so they need
-- no recalculation for this.
ALTER TABLE t_obscalc RENAME COLUMN c_acq_exposure_time_violations TO c_acq_step_violations;
ALTER TABLE t_obscalc RENAME COLUMN c_sci_exposure_time_violations TO c_sci_step_violations;

UPDATE t_obscalc
   SET c_acq_step_violations = exposure_time_to_step_violations(c_acq_step_violations),
       c_sci_step_violations = exposure_time_to_step_violations(c_sci_step_violations)
 WHERE c_acq_step_violations IS NOT NULL
    OR c_sci_step_violations IS NOT NULL;

DROP FUNCTION exposure_time_to_step_violations(jsonb);

-- Cached digests are keyed by a hash that includes the commit, so none is read
-- again after this release is deployed.  Drop them rather than convert them.
ALTER TABLE t_execution_digest RENAME COLUMN c_acq_exposure_time_violations TO c_acq_step_violations;
ALTER TABLE t_execution_digest RENAME COLUMN c_sci_exposure_time_violations TO c_sci_step_violations;

DELETE FROM t_execution_digest;

-- Recalculate the GMOS spectroscopy observations so that they are checked for
-- second order overlap, where it would matter: those with a sequence that are
-- not finished.  Completed and inactive ones are skipped, as in V1340.
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
