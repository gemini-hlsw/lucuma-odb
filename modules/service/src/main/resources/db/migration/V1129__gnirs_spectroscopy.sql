--- GNIRS FPU slit
CREATE TABLE t_gnirs_fpu_slit (
  c_tag        d_tag       NOT NULL PRIMARY KEY,
  c_short_name varchar     NOT NULL,
  c_long_name  varchar     NOT NULL,
  c_slit_width d_angle_µas NOT NULL
);

INSERT INTO t_gnirs_fpu_slit VALUES ('LongSlit_0_10',  '0.10"',  '0.10 arcsec',  100000);
INSERT INTO t_gnirs_fpu_slit VALUES ('LongSlit_0_15',  '0.15"',  '0.15 arcsec',  150000);
INSERT INTO t_gnirs_fpu_slit VALUES ('LongSlit_0_20',  '0.20"',  '0.20 arcsec',  200000);
INSERT INTO t_gnirs_fpu_slit VALUES ('LongSlit_0_30',  '0.30"',  '0.30 arcsec',  300000);
INSERT INTO t_gnirs_fpu_slit VALUES ('LongSlit_0_45',  '0.45"',  '0.45 arcsec',  450000);
INSERT INTO t_gnirs_fpu_slit VALUES ('LongSlit_0_675', '0.675"', '0.675 arcsec', 675000);
INSERT INTO t_gnirs_fpu_slit VALUES ('LongSlit_1_00',  '1.0"',   '1.0 arcsec',   1000000);

--- GNIRS grating
CREATE TABLE t_gnirs_grating (
  c_tag            d_tag    NOT NULL PRIMARY KEY,
  c_short_name     varchar  NOT NULL,
  c_long_name      varchar  NOT NULL,
  c_ruling_density smallint NOT NULL CHECK (c_ruling_density > 0)
);

INSERT INTO t_gnirs_grating VALUES ('D10',  '10',  '10 l/mm grating',  10);
INSERT INTO t_gnirs_grating VALUES ('D32',  '32',  '32 l/mm grating',  32);
INSERT INTO t_gnirs_grating VALUES ('D111', '111', '111 l/mm grating', 111);

--- GNIRS filter
CREATE TABLE t_gnirs_filter (
  c_tag                    d_tag           NOT NULL PRIMARY KEY,
  c_short_name             varchar         NOT NULL,
  c_long_name              varchar         NOT NULL,
  c_wavelength             d_wavelength_pm,
  c_spectroscopy_range_min d_wavelength_pm,
  c_spectroscopy_range_max d_wavelength_pm
);

INSERT INTO t_gnirs_filter VALUES ('CrossDispersed', 'XD',        'Cross dispersed',      NULL,    NULL,    NULL);
INSERT INTO t_gnirs_filter VALUES ('Order6',         'X',         'Order 6 (X)',          1100000, 1103000, 1175400);
INSERT INTO t_gnirs_filter VALUES ('Order5',         'J',         'Order 5 (J)',          1250000, 1175400, 1370000);
INSERT INTO t_gnirs_filter VALUES ('Order4',         'H',         'Order 4 (H: 1.65µm)',  1650000, 1490000, 1800000);
INSERT INTO t_gnirs_filter VALUES ('Order3',         'K',         'Order 3 (K)',          2200000, 1910000, 2490000);
INSERT INTO t_gnirs_filter VALUES ('Order2',         'L',         'Order 2 (L)',          3500000, 2800000, 4200000);
INSERT INTO t_gnirs_filter VALUES ('Order1',         'M',         'Order 1 (M)',          4800000, 4400000, 6000000);
INSERT INTO t_gnirs_filter VALUES ('H2',             'H2',        'H2: 2.12µm',           2120000, NULL,    NULL);
INSERT INTO t_gnirs_filter VALUES ('HNd100x',        'H+ND100X',  'H + ND100X',           1650000, NULL,    NULL);
INSERT INTO t_gnirs_filter VALUES ('H2Nd100x',       'H2+ND100X', 'H2 + ND100X',          2120000, NULL,    NULL);
INSERT INTO t_gnirs_filter VALUES ('PAH',            'PAH',       'PAH: 3.3µm',           3300000, NULL,    NULL);
INSERT INTO t_gnirs_filter VALUES ('Y',              'Y',         'Y: 1.03µm',            1030000, NULL,    NULL);
INSERT INTO t_gnirs_filter VALUES ('J',              'J',         'J: 1.25µm',            1250000, NULL,    NULL);
INSERT INTO t_gnirs_filter VALUES ('K',              'K',         'K: 2.20µm',            2200000, NULL,    NULL);

CREATE TABLE t_spectroscopy_config_option_gnirs (
  c_instrument d_tag NOT NULL DEFAULT ('Gnirs'),
  CHECK (c_instrument = 'Gnirs'),

  c_index      int4  NOT NULL,

  PRIMARY KEY (c_instrument, c_index),
  FOREIGN KEY (c_instrument, c_index) REFERENCES t_spectroscopy_config_option (c_instrument, c_index),

  c_grating    d_tag NOT NULL REFERENCES t_gnirs_grating(c_tag),
  c_filter     d_tag NOT NULL REFERENCES t_gnirs_filter(c_tag),
  c_fpu        d_tag NOT NULL REFERENCES t_gnirs_fpu_slit(c_tag)
);
