-- ItcResult gained a peakPixelFlux field.  The cache key is an md5 of the ITC
-- *input*, so a new output field invalidates nothing, let's remove existing results.
--
-- Frozen rows are spared.
DELETE FROM t_itc_result WHERE NOT c_is_frozen;

UPDATE t_obscalc SET
  c_last_invalidation = NOW(),
  c_failure_count     = 0,
  c_retry_at          = NULL,
  c_obscalc_state     = 'pending'
WHERE c_obscalc_state IN ('ready', 'retry')
  AND c_workflow_state NOT IN ('inactive', 'ongoing', 'completed')
  AND (
        c_workflow_state = 'ready'
        OR c_workflow_validations @> '[{"code": "ITC_ERROR"}]'::jsonb
      );
