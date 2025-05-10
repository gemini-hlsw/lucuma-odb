-- Imaging Configuration Options
CREATE TABLE t_imaging_config_option (
  c_instrument           d_tag           NOT NULL REFERENCES t_instrument(c_tag),
  c_index                int4            NOT NULL,

  PRIMARY KEY (c_instrument, c_index),

  c_ao                   boolean         NOT NULL DEFAULT false,
  c_site                 e_site          NOT NULL

);
COMMENT ON TABLE t_spectroscopy_config_option IS 'Imaging Configuration Option';

CREATE TABLE t_imaging_config_option_gmos_north (
  c_instrument d_tag NOT NULL DEFAULT ('GmosNorth'),
  CHECK (c_instrument = 'GmosNorth'),

  c_index      int4  NOT NULL,

  PRIMARY KEY (c_instrument, c_index),
  FOREIGN KEY (c_instrument, c_index) REFERENCES t_imaging_config_option (c_instrument, c_index),

  c_filter     d_tag          REFERENCES t_gmos_north_filter(c_tag)
);

