-- Add guide state columns for digest offsets
-- Each array is parallel to the corresponding offsets array

ALTER TABLE t_execution_digest
  ADD COLUMN c_acq_offset_guide_states e_guide_state[],
  ADD COLUMN c_sci_offset_guide_states e_guide_state[];

ALTER TABLE t_obscalc
  ADD COLUMN c_acq_offset_guide_states e_guide_state[],
  ADD COLUMN c_sci_offset_guide_states e_guide_state[];

-- Clear existing cached digests so they get recomputed with guide states
DELETE FROM t_execution_digest;

-- Clear existing obscalc cache so it gets recomputed with guide states
UPDATE t_obscalc SET
  c_acq_offsets = NULL,
  c_acq_offset_guide_states = NULL,
  c_sci_offsets = NULL,
  c_sci_offset_guide_states = NULL;
