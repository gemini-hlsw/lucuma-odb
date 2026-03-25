CREATE TABLE t_ghost_binning (
  c_tag          d_tag PRIMARY KEY,
  c_name         text  NOT NULL,
  c_spectral_bin int2  NOT NULL,
  c_spatial_bin  int2  NOT NULL
);

INSERT INTO t_ghost_binning VALUES ('one_by_one',   '1x1', 1, 1);
INSERT INTO t_ghost_binning VALUES ('one_by_two',   '1x2', 1, 2);
INSERT INTO t_ghost_binning VALUES ('one_by_four',  '1x4', 1, 4);
INSERT INTO t_ghost_binning VALUES ('one_by_eight', '1x8', 1, 8);
INSERT INTO t_ghost_binning VALUES ('two_by_two',   '2x2', 2, 2);
INSERT INTO t_ghost_binning VALUES ('two_by_four',  '2x4', 2, 4);
INSERT INTO t_ghost_binning VALUES ('two_by_eight', '2x8', 2, 8);
INSERT INTO t_ghost_binning VALUES ('four_by_four', '4x4', 4, 4);

CREATE TABLE t_ghost_read_mode (
  c_tag        d_tag PRIMARY KEY,
  c_short_name text  NOT NULL,
  c_long_name  text  NOT NULL,
  c_read_rate  int2  NOT NULL
);

INSERT INTO t_ghost_read_mode VALUES ('slow',   'Slow',   'Slow Readout',   10);
INSERT INTO t_ghost_read_mode VALUES ('medium', 'Medium', 'Medium Readout',  5);
INSERT INTO t_ghost_read_mode VALUES ('fast',   'Fast',   'Fast Readout',    2);

CREATE TABLE t_ghost_resolution_mode (
  c_tag        d_tag PRIMARY KEY,
  c_short_name text  NOT NULL,
  c_long_name  text  NOT NULL
);

INSERT INTO t_ghost_resolution_mode VALUES ('standard', 'Standard', 'Standard Resolution');
INSERT INTO t_ghost_resolution_mode VALUES ('high',     'High',     'High Resolution'    );

CREATE TABLE t_spectroscopy_config_option_ghost (
  c_instrument d_tag NOT NULL DEFAULT ('Ghost'),
  CHECK (c_instrument = 'Ghost'),

  c_index      int4 NOT NULL,
  PRIMARY KEY (c_instrument, c_index),
  FOREIGN KEY (c_instrument, c_index) REFERENCES t_spectroscopy_config_option (c_instrument, c_index),

  c_binning         d_tag NOT NULL REFERENCES t_ghost_binning(c_tag),
  c_resolution_mode d_tag NOT NULL REFERENCES t_ghost_resolution_mode(c_tag)

);