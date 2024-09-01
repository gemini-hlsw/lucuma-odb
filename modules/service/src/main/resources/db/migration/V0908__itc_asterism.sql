-- Let's drop the itc values, they will be recalculated soon
TRUNCATE TABLE t_itc_result;

-- Drop individual columns
ALTER TABLE t_itc_result
  DROP CONSTRAINT t_itc_result_pkey,
  DROP COLUMN c_target_id,
  DROP COLUMN c_sci_exposure_time,
  DROP COLUMN c_sci_exposure_count,
  DROP COLUMN c_sci_signal_to_noise,
  DROP COLUMN c_acq_exposure_time,
  DROP COLUMN c_acq_exposure_count,
  DROP COLUMN c_acq_signal_to_noise;

-- Add blob column for all target results in observation
ALTER TABLE t_itc_result
  ADD COLUMN c_asterism_results jsonb not null,
  ADD FOREIGN KEY (c_program_id, c_observation_id)
    REFERENCES t_observation(c_program_id, c_observation_id)
    ON DELETE CASCADE,
  ADD CONSTRAINT t_itc_result_pkey PRIMARY KEY (c_program_id, c_observation_id);