CREATE TYPE e_obscalc_state AS ENUM(
  'pending',
  'calculating',
  'ready'
);

CREATE TABLE t_obscalc(
  c_observation_id       d_observation_id    PRIMARY KEY REFERENCES t_observation(c_observation_id) ON DELETE CASCADE,
  c_obscalc_state        e_obscalc_state     NOT NULL DEFAULT 'pending',
  c_last_invalidation    timestamp           NOT NULL DEFAULT NOW(),
  c_last_update          timestamp           NOT NULL DEFAULT NOW(),

  c_itc_result           jsonb,

  -- Setup time
  c_full_setup_time      interval,
  c_reacq_setup_time     interval,

  -- Acquisition Digest
  c_acq_obs_class        e_obs_class,
  c_acq_non_charged_time interval,
  c_acq_program_time     interval,
  c_acq_offsets          int8[][],
  c_acq_atom_count       int4                CHECK (c_acq_atom_count >= 0),
  c_acq_execution_state  e_execution_state   NOT NULL DEFAULT 'not_defined',

  -- Science Digest
  c_sci_obs_class        e_obs_class,
  c_sci_non_charged_time interval,
  c_sci_program_time     interval,
  c_sci_offsets          int8[][],
  c_sci_atom_count       int4                CHECK (c_sci_atom_count >= 0),
  c_sci_execution_state  e_execution_state   NOT NULL DEFAULT 'not_defined'
);