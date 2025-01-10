
--- Flamingos 2 enumerations

--- Flamingos 2 Decker
create table t_f2_decker (
  c_tag        d_tag           not null primary key,
  c_short_name varchar         not null,
  c_long_name  varchar         not null,
  c_obsolete   bool            not null
);

insert into t_f2_decker values ('Imaging',  'Imaging',   'Imaging',  false);
insert into t_f2_decker values ('LongSlit', 'Long Slit', 'LongSlit', false);
insert into t_f2_decker values ('MOS',      'MOS',       'MOS',      false);

--- Flamingos 2 FPU
create table t_f2_fpu (
  c_tag        d_tag           not null primary key,
  c_short_name varchar         not null,
  c_long_name  varchar         not null,
  c_slit_width smallint        not null,
  c_decker     d_tag           not null references t_f2_decker(c_tag),
  c_obsolete   bool            not null
);

insert into t_f2_fpu values ('Pinhole',       'Pinhole',         '2-Pixel Pinhole Grid', 0, 'Imaging', false);
insert into t_f2_fpu values ('SubPixPinhole', 'Sub-Pix Pinhole', 'Sub-Pixel Pinhole Gr', 0, 'Imaging', false);
insert into t_f2_fpu values ('LongSlit1',     'Long Slit 1px',   '1-Pixel Long Slit',    1, 'LongSlit', false);
insert into t_f2_fpu values ('LongSlit2',     'Long Slit 2px',   '2-Pixel Long Slit',    2, 'LongSlit', false);
insert into t_f2_fpu values ('LongSlit3',     'Long Slit 3px',   '3-Pixel Long Slit',    3, 'LongSlit', false);
insert into t_f2_fpu values ('LongSlit4',     'Long Slit 4px',   '4-Pixel Long Slit',    4, 'LongSlit', false);
insert into t_f2_fpu values ('LongSlit6',     'Long Slit 6px',   '6-Pixel Long Slit',    6, 'LongSlit', false);
insert into t_f2_fpu values ('LongSlit8',     'Long Slit 8px',   '8-Pixel Long Slit',    8, 'LongSlit', false);

--- Flamingos 2 Custom Slit Width
create table t_f2_custom_slit_width (
  c_tag        d_tag           not null primary key,
  c_short_name varchar         not null,
  c_long_name  varchar         not null,
  c_fpu        d_tag           references t_f2_fpu(c_tag)
);

insert into t_f2_custom_slit_width values ('CustomWidth_1_pix', '1 pix', 'CustomWidth 1 Pixel', 'LongSlit1');
insert into t_f2_custom_slit_width values ('CustomWidth_2_pix', '2 pix', 'CustomWidth 2 Pixel', 'LongSlit2');
insert into t_f2_custom_slit_width values ('CustomWidth_3_pix', '3 pix', 'CustomWidth 3 Pixel', 'LongSlit3');
insert into t_f2_custom_slit_width values ('CustomWidth_4_pix', '4 pix', 'CustomWidth 4 Pixel', 'LongSlit4');
insert into t_f2_custom_slit_width values ('CustomWidth_6_pix', '6 pix', 'CustomWidth 6 Pixel', 'LongSlit6');
insert into t_f2_custom_slit_width values ('CustomWidth_8_pix', '8 pix', 'CustomWidth 8 Pixel', 'LongSlit8');;
insert into t_f2_custom_slit_width values ('Other',             'Other', 'Other',               null);

--- Flamingos 2 Disperser
create table t_f2_disperser (
  c_tag        d_tag           not null primary key,
  c_short_name varchar         not null,
  c_long_name  varchar         not null,
  c_wavelength d_wavelength_pm
);

insert into t_f2_disperser values ('R1200JH', 'R1200JH', 'R=1200 (J + H) grism',       1390000);
insert into t_f2_disperser values ('R1200HK', 'R1200HK', 'R=1200 (H + K) grism',       1871000);
insert into t_f2_disperser values ('R3000',   'R3000',   'R=3000 (J or H or K) grism', 1650000);


--- Flamingos 2 Filter
create table t_f2_filter (
  c_tag        d_tag           not null primary key,
  c_short_name varchar         not null,
  c_long_name  varchar         not null,
  c_wavelength d_wavelength_pm,
  c_obsolete   bool            not null
);

insert into t_f2_filter values ('Y',      'Y',       'Y (1.02 um)',        1020000, false);
insert into t_f2_filter values ('F1056',  'F1056',   'F1056 (1.056 um)',   1056000, false);
insert into t_f2_filter values ('J',      'J',       'J (1.25 um)',        1250000, false);
insert into t_f2_filter values ('H',      'H',       'H (1.65 um)',        1650000, false);
insert into t_f2_filter values ('JH',     'JH',      'JH (spectroscopic)', 1390000, false);
insert into t_f2_filter values ('HK',     'HK',      'HK (spectroscopic)', 1871000, false);
insert into t_f2_filter values ('JLow',   'J-low',   'J-low (1.15 um)',    1150000, false);
insert into t_f2_filter values ('KLong',  'K-long',  'K-long (2.20 um)',   2200000, false);
insert into t_f2_filter values ('KShort', 'K-short', 'K-short (2.15 um)',  2150000, false);
insert into t_f2_filter values ('F1063',  'F1063',   'F1063 (1.063 um)',   1063000, false);
insert into t_f2_filter values ('KBlue',  'K-blue',  'K-blue (2.06 um)',   2060000, false);
insert into t_f2_filter values ('KRed',   'K-red',   'K-red (2.31 um)',    2310000, false);
insert into t_f2_filter values ('Open',   'Open',    'Open',               1600000, true);
insert into t_f2_filter values ('Dark',   'Dark',    'Dark',               null,    true);

--- Flamingos 2 Lyot Wheel
create table t_f2_lyot_wheel (
  c_tag         d_tag   not null  primary key,
  c_short_name  varchar not null,
  c_long_name   varchar not null,
  c_plate_scale real    not null, -- arcsec/mm
  c_pixel_scale real    not null, -- arcsec/pixel
  c_obsolete    bool    not null
);

insert into t_f2_lyot_wheel values ('F16',       'f/16',            'f/16 (Open)',               1.61,  0.18, false);
insert into t_f2_lyot_wheel values ('F32High',   'f/32 High',       'f/32 MCAO high background', 0.805, 0.09, true);
insert into t_f2_lyot_wheel values ('F32Low',    'f/32 Low',        'f/32 MCAO low background',  0.805, 0.09, true);
insert into t_f2_lyot_wheel values ('F33Gems',   'f/33 GeMS',       'f/33 (GeMS)',               0.784, 0.09, true);
insert into t_f2_lyot_wheel values ('GemsUnder', 'GeMS Under',      'f/33 (GeMS under-sized)',   0.784, 0.09, false);
insert into t_f2_lyot_wheel values ('GemsOver',  'GeMS Over',       'f/33 (GeMS over-sized)',    0.784, 0.09, false);
insert into t_f2_lyot_wheel values ('HartmannA', 'Hartmann A (H1)', 'Hartmann A (H1)',           0.0,   0.0,  false);
insert into t_f2_lyot_wheel values ('HartmannB', 'Hartmann B (H2)', 'Hartmann B (H2)',           0.0,   0.0,  false);

--- Flamingos 2 Read Mode
create table t_f2_readout_mode (
  c_tag                       d_tag    not null primary key,
  c_short_name                varchar  not null,
  c_long_name                 varchar  not null,
  c_description               varchar  not null,
  c_min_exposure_time         interval not null,
  c_recommended_exposure_time interval not null,
  c_readout_time              interval not null,
  c_read_count                smallint not null check   (c_read_count > 0),
  c_read_noise                real     not null check   (c_read_noise > 0.0)
);

insert into t_f2_readout_mode values ('Bright', 'bright', 'Bright Object', 'Strong Source', '1500 millisecond', '5000 millisecond', '8000 millisecond', 1, 11.7);
insert into t_f2_readout_mode values ('Medium', 'medium', 'Medium Object', 'Medium Source', '6 second',         '21 second',        '14 second',        4, 6.0);
insert into t_f2_readout_mode values ('Faint',  'faint',  'Faint Object',  'Weak Source',   '12 second',        '85 second',        '20 second',        8, 5.0);

--- Flamingos 2 Readout Mode
create table f2_readout_mode (
  c_tag        d_tag   not null primary key,
  c_short_name varchar not null,
  c_long_name  varchar not null
);

insert into f2_readout_mode values ('Science',     'Science',     'Science');
insert into f2_readout_mode values ('Engineering', 'Engineering', 'Engineering');

--- Flamingos 2 Reads
create table t_f2_reads (
  c_tag        d_tag    not null primary key,
  c_short_name varchar  not null,
  c_long_name  varchar  not null,
  c_reads      smallint not null check   (c_reads > 0)
);

insert into t_f2_reads values ('reads_1',  'reads_1',  'Reads 1',  1);
insert into t_f2_reads values ('reads_3',  'reads_3',  'Reads 3',  3);
insert into t_f2_reads values ('reads_4',  'reads_4',  'Reads 4',  4);
insert into t_f2_reads values ('reads_5',  'reads_5',  'Reads 5',  5);
insert into t_f2_reads values ('reads_6',  'reads_6',  'Reads 6',  6);
insert into t_f2_reads values ('reads_7',  'reads_7',  'Reads 7',  7);
insert into t_f2_reads values ('reads_8',  'reads_8',  'Reads 8',  8);
insert into t_f2_reads values ('reads_9',  'reads_9',  'Reads 9',  9);
insert into t_f2_reads values ('reads_10', 'reads_10', 'Reads 10', 10);
insert into t_f2_reads values ('reads_11', 'reads_11', 'Reads 11', 11);
insert into t_f2_reads values ('reads_12', 'reads_12', 'Reads 12', 12);
insert into t_f2_reads values ('reads_13', 'reads_13', 'Reads 13', 13);
insert into t_f2_reads values ('reads_14', 'reads_14', 'Reads 14', 14);
insert into t_f2_reads values ('reads_15', 'reads_15', 'Reads 15', 15);
insert into t_f2_reads values ('reads_16', 'reads_16', 'Reads 16', 16);

--- Flamingos 2 Window Cover
create table t_f2_window_cover (
  c_tag        d_tag   not null primary key,
  c_short_name varchar not null,
  c_long_name  varchar not null
);

insert into t_f2_window_cover values ('Open',  'Open',  'Open');
insert into t_f2_window_cover values ('Close', 'Close', 'Close');

CREATE TABLE t_spectroscopy_config_option_f2 (
  c_instrument d_tag NOT NULL DEFAULT ('Flamingos2'),
  CHECK (c_instrument = 'Flamingos2'),

  c_index      int4  NOT NULL,

  PRIMARY KEY (c_instrument, c_index),
  FOREIGN KEY (c_instrument, c_index) REFERENCES t_spectroscopy_config_option (c_instrument, c_index),

  c_fpu        d_tag NOT NULL REFERENCES t_f2_fpu(c_tag),
  c_grating    d_tag NOT NULL REFERENCES t_f2_disperser(c_tag),
  c_filter     d_tag          REFERENCES t_f2_filter(c_tag)
);

