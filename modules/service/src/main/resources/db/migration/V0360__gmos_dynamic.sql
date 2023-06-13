-- Updates to GMOS North and South dynamic config

-- Should ALTER TABLE but it is currently unused and it's easier to redefine.
DROP TABLE t_gmos_north_dynamic;

CREATE TABLE t_gmos_north_dynamic (

  c_step_id    d_step_id    PRIMARY KEY,
  c_instrument d_tag        NOT NULL DEFAULT ('GmosNorth'),

  FOREIGN KEY (c_step_id, c_instrument)
  REFERENCES t_step (c_step_id, c_instrument),
  CHECK (c_instrument = 'GmosNorth'),

  c_exposure_time interval NOT NULL,

  c_xbin          d_tag    NOT NULL REFERENCES t_gmos_binning(c_tag),
  c_ybin          d_tag    NOT NULL REFERENCES t_gmos_binning(c_tag),
  c_amp_count     d_tag    NOT NULL REFERENCES t_gmos_amp_count(c_tag),
  c_amp_gain      d_tag    NOT NULL REFERENCES t_gmos_amp_gain(c_tag),
  c_amp_read_mode d_tag    NOT NULL REFERENCES t_gmos_amp_read_mode(c_tag),

  c_dtax          d_tag    NOT NULL REFERENCES t_gmos_dtax(c_tag),
  c_roi           d_tag    NOT NULL REFERENCES t_gmos_roi(c_tag),

  -- all null or all non-null
  c_grating_disperser      d_tag    REFERENCES t_gmos_north_disperser(c_tag),
  c_grating_order          d_tag    REFERENCES t_gmos_disperser_order(c_tag),
  c_grating_wavelength     d_wavelength_pm,
  CHECK (
    (
      c_grating_disperser  IS NULL AND
      c_grating_order      IS NULL AND
      c_grating_wavelength IS NULL
    ) OR (
      c_grating_disperser  IS NOT NULL AND
      c_grating_order      IS NOT NULL AND
      c_grating_wavelength IS NOT NULL
    )
  ),

  c_filter d_tag REFERENCES t_gmos_north_filter(c_tag),

  c_fpu_custom_mask_filename   varchar  NULL CHECK(length(c_fpu_custom_mask_filename) > 0),
  c_fpu_custom_mask_slit_width d_tag    NULL REFERENCES t_gmos_custom_slit_width(c_tag),
  c_fpu_builtin                d_tag    NULL REFERENCES t_gmos_north_fpu(c_tag),
  CONSTRAINT gmos_north_fpu_check CHECK (
    (
      c_fpu_custom_mask_filename   IS NULL AND
      c_fpu_custom_mask_slit_width IS NULL
    ) OR (
      c_fpu_custom_mask_filename   IS NOT NULL AND
      c_fpu_custom_mask_slit_width IS NOT NULL AND
      c_fpu_builtin IS NULL
    )
  )

);

DROP TYPE e_gmos_fpu_option;

CREATE TABLE t_gmos_south_dynamic (

  c_step_id    d_step_id    PRIMARY KEY,
  c_instrument d_tag        NOT NULL DEFAULT ('GmosSouth'),

  FOREIGN KEY (c_step_id, c_instrument)
  REFERENCES t_step (c_step_id, c_instrument),
  CHECK (c_instrument = 'GmosSouth'),

  c_exposure_time interval NOT NULL,

  c_xbin          d_tag    NOT NULL REFERENCES t_gmos_binning(c_tag),
  c_ybin          d_tag    NOT NULL REFERENCES t_gmos_binning(c_tag),
  c_amp_count     d_tag    NOT NULL REFERENCES t_gmos_amp_count(c_tag),
  c_amp_gain      d_tag    NOT NULL REFERENCES t_gmos_amp_gain(c_tag),
  c_amp_read_mode d_tag    NOT NULL REFERENCES t_gmos_amp_read_mode(c_tag),

  c_dtax          d_tag    NOT NULL REFERENCES t_gmos_dtax(c_tag),
  c_roi           d_tag    NOT NULL REFERENCES t_gmos_roi(c_tag),

  -- all null or all non-null
  c_grating_disperser      d_tag    REFERENCES t_gmos_south_disperser(c_tag),
  c_grating_order          d_tag    REFERENCES t_gmos_disperser_order(c_tag),
  c_grating_wavelength     d_wavelength_pm,
  CHECK (
    (
      c_grating_disperser  IS NULL AND
      c_grating_order      IS NULL AND
      c_grating_wavelength IS NULL
    ) OR (
      c_grating_disperser  IS NOT NULL AND
      c_grating_order      IS NOT NULL AND
      c_grating_wavelength IS NOT NULL
    )
  ),

  c_filter d_tag REFERENCES t_gmos_south_filter(c_tag),

  c_fpu_custom_mask_filename   varchar  NULL CHECK(length(c_fpu_custom_mask_filename) > 0),
  c_fpu_custom_mask_slit_width d_tag    NULL REFERENCES t_gmos_custom_slit_width(c_tag),
  c_fpu_builtin                d_tag    NULL REFERENCES t_gmos_south_fpu(c_tag),
  CONSTRAINT gmos_south_fpu_check CHECK (
    (
      c_fpu_custom_mask_filename   IS NULL AND
      c_fpu_custom_mask_slit_width IS NULL
    ) OR (
      c_fpu_custom_mask_filename   IS NOT NULL AND
      c_fpu_custom_mask_slit_width IS NOT NULL AND
      c_fpu_builtin IS NULL
    )
  )

);  