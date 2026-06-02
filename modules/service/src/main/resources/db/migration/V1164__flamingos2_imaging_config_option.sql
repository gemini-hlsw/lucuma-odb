
CREATE TABLE t_imaging_config_option_flamingos_2 (
  c_instrument d_tag NOT NULL DEFAULT ('Flamingos2'),
  CHECK (c_instrument = 'Flamingos2'),

  c_index      int4  NOT NULL,

  PRIMARY KEY (c_instrument, c_index),
  FOREIGN KEY (c_instrument, c_index) REFERENCES t_imaging_config_option (c_instrument, c_index),

  c_filter     d_tag          REFERENCES t_f2_filter(c_tag)
);
