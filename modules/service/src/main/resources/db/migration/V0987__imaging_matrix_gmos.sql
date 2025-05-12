
-- Missing GmosS filters
insert into t_gmos_south_filter values ('OVI', 'OVI', 'OVI_G0347', 684000, '[681600, 696500]', 'NarrowBand');
insert into t_gmos_south_filter values ('OVIC', 'OVIC', 'OVI_G0349', 679000, '[679000, 680500]', 'NarrowBand');

-- Imaging Configuration Options
CREATE TABLE t_imaging_config_option (
  c_instrument           d_tag           NOT NULL REFERENCES t_instrument(c_tag),
  c_index                int4            NOT NULL,

  PRIMARY KEY (c_instrument, c_index),

  c_fov                  d_angle_µas     NOT NULL,
  c_filter_label         text            NOT NULL CHECK (length(c_filter_label) > 0),
  c_ao                   boolean         NOT NULL DEFAULT false,
  c_site                 e_site          NOT NULL

);
COMMENT ON TABLE t_imaging_config_option IS 'Imaging Configuration Options';

CREATE TABLE t_imaging_config_option_gmos_north (
  c_instrument d_tag NOT NULL DEFAULT ('GmosNorth'),
  CHECK (c_instrument = 'GmosNorth'),

  c_index      int4  NOT NULL,

  PRIMARY KEY (c_instrument, c_index),
  FOREIGN KEY (c_instrument, c_index) REFERENCES t_imaging_config_option (c_instrument, c_index),

  c_filter     d_tag          REFERENCES t_gmos_north_filter(c_tag)
);

COMMENT ON TABLE t_imaging_config_option_gmos_north IS 'Imaging Configuration Options for Gmos North';

CREATE TABLE t_imaging_config_option_gmos_south (
  c_instrument d_tag NOT NULL DEFAULT ('GmosSouth'),
  CHECK (c_instrument = 'GmosSouth'),

  c_index      int4  NOT NULL,

  PRIMARY KEY (c_instrument, c_index),
  FOREIGN KEY (c_instrument, c_index) REFERENCES t_imaging_config_option (c_instrument, c_index),

  c_filter     d_tag          REFERENCES t_gmos_south_filter(c_tag)
);

COMMENT ON TABLE t_imaging_config_option_gmos_south IS 'Imaging Configuration Options for Gmos South';
