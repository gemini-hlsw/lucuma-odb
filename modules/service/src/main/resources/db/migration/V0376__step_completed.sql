
-- Add a completion time that is set when the step is complete, but otherwise null
ALTER TABLE t_step_record
  ADD COLUMN c_completed timestamp NULL;