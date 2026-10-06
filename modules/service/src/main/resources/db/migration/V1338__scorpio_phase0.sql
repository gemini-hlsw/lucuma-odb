--- SCORPIO long slit FPU
CREATE TABLE t_scorpio_fpu (
  c_tag        d_tag       NOT NULL PRIMARY KEY,
  c_short_name varchar     NOT NULL,
  c_long_name  varchar     NOT NULL,
  c_slit_width d_angle_µas NOT NULL
);

INSERT INTO t_scorpio_fpu VALUES ('LongSlit_0_36', '0.36"', 'Longslit 0.36 arcsec',  360000);
INSERT INTO t_scorpio_fpu VALUES ('LongSlit_0_54', '0.54"', 'Longslit 0.54 arcsec',  540000);
INSERT INTO t_scorpio_fpu VALUES ('LongSlit_0_72', '0.72"', 'Longslit 0.72 arcsec',  720000);
INSERT INTO t_scorpio_fpu VALUES ('LongSlit_1_08', '1.08"', 'Longslit 1.08 arcsec', 1080000);
INSERT INTO t_scorpio_fpu VALUES ('LongSlit_1_44', '1.44"', 'Longslit 1.44 arcsec', 1440000);
INSERT INTO t_scorpio_fpu VALUES ('LongSlit_2_16', '2.16"', 'Longslit 2.16 arcsec', 2160000);
INSERT INTO t_scorpio_fpu VALUES ('LongSlit_4_32', '4.32"', 'Longslit 4.32 arcsec', 4320000);

--- SCORPIO filters (fixed dichroic channels)
CREATE TABLE t_scorpio_filter (
  c_tag        d_tag           NOT NULL PRIMARY KEY,
  c_short_name varchar         NOT NULL,
  c_long_name  varchar         NOT NULL,
  c_wavelength d_wavelength_pm NOT NULL
);

INSERT INTO t_scorpio_filter VALUES ('G',  'g',  'g_G1301',  469000);
INSERT INTO t_scorpio_filter VALUES ('R',  'r',  'r_G1302',  622000);
INSERT INTO t_scorpio_filter VALUES ('I',  'i',  'i_G1303',  755000);
INSERT INTO t_scorpio_filter VALUES ('Z',  'z',  'z_G1304',  889000);
INSERT INTO t_scorpio_filter VALUES ('Y',  'Y',  'Y_G1305', 1040000);
INSERT INTO t_scorpio_filter VALUES ('J',  'J',  'J_G1306', 1250000);
INSERT INTO t_scorpio_filter VALUES ('H',  'H',  'H_G1307', 1630000);
INSERT INTO t_scorpio_filter VALUES ('Ks', 'Ks', 'Ks_G1308', 2175000);

--- Phase 0 spectroscopy options. SCORPIO exposes every channel at once, so an
--- option is identified by its slit alone.
CREATE TABLE t_spectroscopy_config_option_scorpio (
  c_instrument d_tag NOT NULL DEFAULT ('Scorpio'),
  CHECK (c_instrument = 'Scorpio'),

  c_index      int4  NOT NULL,

  PRIMARY KEY (c_instrument, c_index),
  FOREIGN KEY (c_instrument, c_index) REFERENCES t_spectroscopy_config_option (c_instrument, c_index),

  c_fpu        d_tag NOT NULL REFERENCES t_scorpio_fpu(c_tag)
);
