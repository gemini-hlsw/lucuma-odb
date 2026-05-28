
CREATE TABLE t_gnirs_static (
  c_static_id       bigserial                  PRIMARY KEY,
  c_observation_id  d_observation_id  NOT NULL REFERENCES t_observation (c_observation_id),
  c_instrument      d_tag             NOT NULL DEFAULT ('Gnirs'),

  FOREIGN KEY (c_observation_id, c_instrument)
  REFERENCES t_observation (c_observation_id, c_instrument),

  CHECK (c_instrument = 'Gnirs'),

  c_well_depth      e_gnirs_well_depth NOT NULL
);

CREATE TABLE t_gnirs_dynamic (
  c_step_id        d_step_id    PRIMARY KEY,
  c_instrument     d_tag        NOT NULL DEFAULT ('Gnirs'),

  FOREIGN KEY (c_step_id, c_instrument)
  REFERENCES t_step (c_step_id, c_instrument) ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED,

  CHECK (c_instrument = 'Gnirs'),

  c_exposure_time      interval        NOT NULL,
  c_coadds             int4            NOT NULL,
  c_central_wavelength d_wavelength_pm NOT NULL,
  c_filter             d_tag           NOT NULL REFERENCES t_gnirs_filter(c_tag),
  c_decker             e_gnirs_decker  NOT NULL,

  -- FPU: exactly one of the long-slit slit (c_fpu_slit) or the non-slit
  -- "other" value (c_fpu_other) is present.
  c_fpu_slit           d_tag           REFERENCES t_gnirs_fpu_slit(c_tag),
  c_fpu_other          d_tag,
  CONSTRAINT gnirs_fpu_check CHECK (
    (c_fpu_slit IS NOT NULL AND c_fpu_other IS     NULL) OR
    (c_fpu_slit IS     NULL AND c_fpu_other IS NOT NULL)
  ),

  -- Spectroscopy config carried when the acquisition mirror is "out" of the
  -- beam.  All three are null together when the mirror is "in".
  c_prism              d_tag           REFERENCES t_gnirs_prism(c_tag),
  c_grating            d_tag           REFERENCES t_gnirs_grating(c_tag),
  c_grating_wavelength d_wavelength_pm,
  CONSTRAINT gnirs_acquisition_mirror_check CHECK (
    (c_prism IS     NULL AND c_grating IS     NULL AND c_grating_wavelength IS     NULL) OR
    (c_prism IS NOT NULL AND c_grating IS NOT NULL AND c_grating_wavelength IS NOT NULL)
  ),

  c_camera             d_tag           NOT NULL REFERENCES t_gnirs_camera(c_tag),

  -- Focus motor steps.  Null means "Best" (instrument-chosen) focus.
  c_focus_motor_steps  int4,

  c_read_mode          d_tag           NOT NULL
);

CREATE VIEW v_gnirs_dynamic AS
  SELECT t.*,
  CASE WHEN t.c_prism IS NOT NULL THEN t.c_step_id END AS c_acquisition_mirror_out_id
FROM
  t_gnirs_dynamic t;
