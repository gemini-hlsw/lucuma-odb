
CREATE TABLE t_imaging_config_option_gnirs (
  c_instrument d_tag NOT NULL DEFAULT ('Gnirs'),
  CHECK (c_instrument = 'Gnirs'),

  c_index      int4  NOT NULL,

  PRIMARY KEY (c_instrument, c_index),
  FOREIGN KEY (c_instrument, c_index) REFERENCES t_imaging_config_option (c_instrument, c_index),

  c_filter     d_tag          REFERENCES t_gnirs_filter(c_tag),
  c_camera     d_tag          REFERENCES t_gnirs_camera(c_tag)
);
