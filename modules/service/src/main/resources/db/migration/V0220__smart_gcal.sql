CREATE TYPE e_gcal_lamp_type AS ENUM (
    'Arc',
    'Flat'
);

CREATE TYPE e_gcal_baseline_type AS ENUM (
    'Day',
    'Night'
);

CREATE TABLE t_gcal_continuum (
    c_tag        d_tag   PRIMARY KEY,
    c_short_name varchar NOT NULL,
    c_long_name  varchar NOT NULL
);

INSERT INTO t_gcal_continuum VALUES ('IrGreyBodyLow',  'IR grey body - low',  'IR grey body - low');
INSERT INTO t_gcal_continuum VALUES ('IrGreyBodyHigh', 'IR grey body - high', 'IR grey body - high');
INSERT INTO t_gcal_continuum VALUES ('QuartzHalogen',  'Quartz Halogen',      'Quartz Halogen');

CREATE TABLE t_gcal_filter (
    c_tag        d_tag   PRIMARY KEY,
    c_short_name varchar NOT NULL,
    c_long_name  varchar NOT NULL
);

INSERT INTO t_gcal_filter VALUES ('None', 'none',         'none');
INSERT INTO t_gcal_filter VALUES ('Gmos', 'GMOS balance', 'GMOS balance');
INSERT INTO t_gcal_filter VALUES ('Nir',  'NIR balance',	'NIR balance');
INSERT INTO t_gcal_filter VALUES ('Nd10', 'ND1.0',        'ND1.0');
INSERT INTO t_gcal_filter VALUES ('Nd20', 'ND2.0',        'ND2.0');
INSERT INTO t_gcal_filter VALUES ('Nd30', 'ND3.0',        'ND3.0');
INSERT INTO t_gcal_filter VALUES ('Nd40', 'ND4.0',        'ND4.0');
INSERT INTO t_gcal_filter VALUES ('Nd45', 'ND4-5',        'ND4-5');

CREATE TABLE t_gcal_diffuser (
    c_tag        d_tag   PRIMARY KEY,
    c_short_name varchar NOT NULL,
    c_long_name  varchar NOT NULL
);

INSERT INTO t_gcal_diffuser VALUES ('Ir',      'IR',      'IR');
INSERT INTO t_gcal_diffuser VALUES ('Visible', 'Visible', 'Visible');

CREATE TABLE t_gcal_shutter (
    c_tag        d_tag   PRIMARY KEY,
    c_short_name varchar NOT NULL,
    c_long_name  varchar NOT NULL
);

INSERT INTO t_gcal_shutter VALUES ('Open',   'Open',   'Open');
INSERT INTO t_gcal_shutter VALUES ('Closed', 'Closed', 'Closed');