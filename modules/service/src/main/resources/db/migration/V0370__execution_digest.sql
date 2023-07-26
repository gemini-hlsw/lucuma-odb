
CREATE TYPE e_obs_class AS ENUM(
  'science',
  'programCal',
  'partnerCal',
  'acquisition',
  'acquisitionCal',
  'dayCal'
);

--
-- Execution digest table, associated with a hash of inputs that went into
-- producing it.
--
CREATE TABLE t_execution_digest (
  -- Primary Key (+ fk to t_observation)
  c_program_id           d_program_id     NOT NULL,
  c_observation_id       d_observation_id NOT NULL,

  -- Hash of inputs used to create this result.
  c_hash                 text             NOT NULL,

  -- Setup time
  c_full_setup_time      interval         NOT NULL,
  c_reacq_setup_time     interval         NOT NULL,

  -- Acquisition Digest
  c_acq_obs_class        e_obs_class      NOT NULL,
  c_acq_non_charged_time interval         NOT NULL,
  c_acq_partner_time     interval         NOT NULL,
  c_acq_program_time     interval         NOT NULL,
  c_acq_offsets          int8[][]         NOT NULL,
  c_acq_atom_count       int4             NOT NULL CHECK (c_acq_atom_count >= 0),

  -- Science Digest
  c_sci_obs_class        e_obs_class      NOT NULL,
  c_sci_non_charged_time interval         NOT NULL,
  c_sci_partner_time     interval         NOT NULL,
  c_sci_program_time     interval         NOT NULL,
  c_sci_offsets          int8[][]         NOT NULL,
  c_sci_atom_count       int4             NOT NULL CHECK (c_sci_atom_count >= 0),

  FOREIGN KEY (c_program_id, c_observation_id)
    REFERENCES t_observation(c_program_id, c_observation_id)
    ON DELETE CASCADE,
  CONSTRAINT t_execution_digest_pkey PRIMARY KEY (c_program_id, c_observation_id)
);