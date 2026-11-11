-- A Smart GCAL row with a step count of zero marks a configuration that is
-- known to have no calibration of that type (sc-11465).
ALTER TABLE t_gcal
  DROP CONSTRAINT check_positive_step_count,
  ADD  CONSTRAINT check_non_negative_step_count CHECK (c_gcal_step_count >= 0);
