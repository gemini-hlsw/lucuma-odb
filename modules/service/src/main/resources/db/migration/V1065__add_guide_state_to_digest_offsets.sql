-- Add guide state columns for digest offsets

ALTER TABLE t_execution_digest
  ADD COLUMN c_acq_offset_guide_states e_guide_state[],
  ADD COLUMN c_sci_offset_guide_states e_guide_state[];

-- each offset needs a guide state
ALTER TABLE t_execution_digest
  ADD CONSTRAINT t_execution_digest_acq_offsets_guide_states_length
    CHECK (array_length(c_acq_offsets, 1) IS NOT DISTINCT FROM array_length(c_acq_offset_guide_states, 1)),
  ADD CONSTRAINT t_execution_digest_sci_offsets_guide_states_length
    CHECK (array_length(c_sci_offsets, 1) IS NOT DISTINCT FROM array_length(c_sci_offset_guide_states, 1));

ALTER TABLE t_obscalc
  ADD COLUMN c_acq_offset_guide_states e_guide_state[],
  ADD COLUMN c_sci_offset_guide_states e_guide_state[];

ALTER TABLE t_obscalc
  ADD CONSTRAINT t_obscalc_acq_offsets_guide_states_length
    CHECK (array_length(c_acq_offsets, 1) IS NOT DISTINCT FROM array_length(c_acq_offset_guide_states, 1)),
  ADD CONSTRAINT t_obscalc_sci_offsets_guide_states_length
    CHECK (array_length(c_sci_offsets, 1) IS NOT DISTINCT FROM array_length(c_sci_offset_guide_states, 1));

-- Clear existing digests
DELETE FROM t_execution_digest;

UPDATE t_obscalc SET
  c_acq_offsets = NULL,
  c_acq_offset_guide_states = NULL,
  c_sci_offsets = NULL,
  c_sci_offset_guide_states = NULL;
