-- Base coordinates outside the Call for Proposals limits are now reported as a
-- dismissable CFP_WARNING rather than a CFP_ERROR.  The workflow state and
-- validations are cached with the obscalc result, so recalculate every
-- observation that recorded the old error.
UPDATE t_obscalc SET
  c_last_invalidation = NOW(),
  c_failure_count     = 0,
  c_retry_at          = NULL,
  c_obscalc_state     = 'pending'
WHERE c_workflow_validations @> '[{"code": "CFP_ERROR", "messages": ["Base coordinates out of Call for Proposals limits."]}]'::jsonb;
