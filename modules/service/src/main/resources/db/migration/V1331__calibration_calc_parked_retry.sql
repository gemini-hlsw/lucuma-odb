-- A row that failed too many times is parked: 'retry' with no retry time.
-- It is not loaded again until an invalidation re-pends it.
ALTER TABLE t_calibration_calc
  DROP CONSTRAINT check_retry_at_defined_for_retry_state;
