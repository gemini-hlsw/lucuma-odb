-- A READY observation with a warning that the program has not dismissed now
-- reverts to DEFINED.  The workflow state is cached with the obscalc result, so
-- recalculate every READY observation that recorded such a warning.  Validation
-- codes are stored upper case in the JSON but dismissals are lower case tags.
UPDATE t_obscalc c SET
  c_last_invalidation = NOW(),
  c_failure_count     = 0,
  c_retry_at          = NULL,
  c_obscalc_state     = 'pending'
FROM t_program p
WHERE p.c_program_id = c.c_program_id
  AND c.c_workflow_state = 'ready'
  AND EXISTS (
    SELECT 1
      FROM jsonb_array_elements(c.c_workflow_validations) v
     WHERE lower(v->>'code') IN (
             'generic_warning',
             'conditions_unlikely',
             'low_total_signal_to_noise',
             'configuration_warning',
             'too_activation_unexpected',
             'cfp_warning'
           )
       AND NOT (lower(v->>'code') = ANY (p.c_dismissed_warnings::text[]))
  );
