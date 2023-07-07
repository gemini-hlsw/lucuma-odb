
CREATE TYPE e_obs_class AS ENUM(
  'science',
  'programCal',
  'partnerCal',
  'acquisition',
  'acquisitionCal',
  'dayCal'
);

--
-- Sequence result table, associated with a hash of inputs that went into
-- producing it.
--
CREATE TABLE t_sequence_result (
  -- Key
  c_observation_id   d_observation_id NOT NULL,
  c_program_id       d_program_id     NOT NULL,
  c_sequence_type    e_sequence_type  NOT NULL,

  -- Hash of inputs used to create this result.
  c_hash             text             NOT NULL,

  -- Execution sequence as JSON.
  c_sequence         jsonb            NOT NULL,

  -- Computed observation class.
  c_obs_class        e_obs_class      NOT NULL,

  -- Planned time.
  c_non_charged_time interval         NOT NULL,
  c_partner_time     interval         NOT NULL,
  c_program_time     interval         NOT NULL,

  -- Unique offset positions
  --c_offset_positions int8[][]         NOT NULL,

  FOREIGN KEY (c_program_id, c_observation_id)
    REFERENCES t_observation(c_program_id, c_observation_id)
    ON DELETE CASCADE,
  CONSTRAINT t_sequence_result_pkey PRIMARY KEY (c_program_id, c_observation_id, c_sequence_type)
);