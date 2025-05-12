CREATE TABLE t_flamingos_2_dynamic (

  c_step_id       d_step_id    PRIMARY KEY,
  c_instrument    d_tag        NOT NULL DEFAULT ('Flamingos2'),

  FOREIGN KEY (c_step_id, c_instrument)
  REFERENCES t_step (c_step_id, c_instrument),
  CHECK (c_instrument = 'Flamingos2'),

  c_exposure_time interval NOT NULL,

  c_disperser     d_tag             REFERENCES t_f2_disperser(c_tag),
  c_filter        d_tag    NOT NULL REFERENCES t_f2_filter(c_tag),
  c_read_mode     d_tag    NOT NULL REFERENCES t_f2_read_mode(c_tag),
  c_lyot_wheel    d_tag    NOT NULL REFERENCES t_f2_lyot_wheel(c_tag),

  c_fpu_custom_mask_filename   varchar  CHECK(length(c_fpu_custom_mask_filename) > 0),
  c_fpu_custom_mask_slit_width d_tag    REFERENCES t_f2_custom_slit_width(c_tag),
  c_fpu_builtin                d_tag    REFERENCES t_f2_fpu(c_tag),
  CONSTRAINT f2_fpu_check CHECK (
    (
      c_fpu_custom_mask_filename   IS NULL AND
      c_fpu_custom_mask_slit_width IS NULL
    ) OR (
      c_fpu_custom_mask_filename   IS NOT NULL AND
      c_fpu_custom_mask_slit_width IS NOT NULL AND
      c_fpu_builtin IS NULL
    )
  ),

  c_readout_mode d_tag              REFERENCES t_f2_readout_mode(c_tag),
  c_reads        c_tag              REFERENCES t_f2_reads(c_tag)
);