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

  c_odb_error            jsonb,

  -- Imaging ITC Result
  c_img_target_id        d_target_id         REFERENCES t_target(c_target_id) ON DELETE CASCADE,
  c_img_exposure_time    interval,
  c_img_exposure_count   int4,
  c_img_wavelength       d_wavelength_pm,
  c_img_single_sn        numeric(10,3),
  c_img_total_sn         numeric(10, 3),

  -- Spectroscopy ITC Result
  c_spec_target_id        d_target_id        REFERENCES t_target(c_target_id) ON DELETE CASCADE,
  c_spec_exposure_time    interval,
  c_spec_exposure_count   int4,
  c_spec_wavelength       d_wavelength_pm,
  c_spec_single_sn        numeric(10,3),
  c_spec_total_sn         numeric(10, 3),

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