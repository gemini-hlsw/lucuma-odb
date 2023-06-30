
--
-- ITC result table associated with a hash of ITC inputs.
--
CREATE TABLE t_itc_result (
  c_program_id      d_program_id     NOT NULL,
  c_observation_id  d_observation_id NOT NULL,
  c_target_id       d_target_id      NOT NULL,
  c_hash            text             NOT NULL,
  c_exposure_time   interval         NOT NULL,
  c_exposure_count  int4             NOT NULL,
  c_signal_to_noise numeric(10,3)    NOT NULL,
  c_version         text             NOT NULL,
  c_data            text             NULL,
  FOREIGN KEY (c_program_id, c_observation_id, c_target_id)
    REFERENCES t_asterism_target(c_program_id, c_observation_id, c_target_id)
    ON DELETE CASCADE,
  CONSTRAINT t_itc_result_pkey PRIMARY KEY (c_program_id, c_observation_id, c_target_id)
)