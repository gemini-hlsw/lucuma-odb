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

INSERT INTO t_gcal_continuum VALUES ('IrGreyBodyLow',    'IR grey body - low',  'IR grey body - low');
INSERT INTO t_gcal_continuum VALUES ('IrGreyBodyHigh',   'IR grey body - high', 'IR grey body - high');
INSERT INTO t_gcal_continuum VALUES ('QuartzHalogen5',   '5W Quartz Halogen',   '5W Quartz Halogen');
INSERT INTO t_gcal_continuum VALUES ('QuartzHalogen100', '100W Quartz Halogen', '100W Quartz Halogen');

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

CREATE TABLE t_gcal (

  -- When we reload smart gcal information, we need to delete the corresponding
  -- gcal configs.  Smart gcal is loaded on a per-instrument basis so this
  -- allows us to delete the appropriate rows.
  c_instrument       d_tag                 REFERENCES t_instrument(c_tag) ON DELETE CASCADE,
  c_gcal_id          int4,
  PRIMARY KEY (c_instrument, c_gcal_id),

  -- Gcal Configuration Value
  c_gcal_continuum   d_tag                          REFERENCES t_gcal_continuum(c_tag) ON DELETE CASCADE,
  c_gcal_ar_arc      boolean               NOT NULL DEFAULT FALSE,
  c_gcal_cuar_arc    boolean               NOT NULL DEFAULT FALSE,
  c_gcal_thar_arc    boolean               NOT NULL DEFAULT FALSE,
  c_gcal_xe_arc      boolean               NOT NULL DEFAULT FALSE,

  -- NOTE: Either a continuum lamp or else one of the arcs, but never both.
  CONSTRAINT check_lamp CHECK ((c_gcal_continuum IS NULL) = (c_gcal_ar_arc OR c_gcal_cuar_arc OR c_gcal_thar_arc OR c_gcal_xe_arc)),

  c_gcal_filter      d_tag                 NOT NULL REFERENCES t_gcal_filter(c_tag)   ON DELETE CASCADE,
  c_gcal_diffuser    d_tag                 NOT NULL REFERENCES t_gcal_diffuser(c_tag) ON DELETE CASCADE,
  c_gcal_shutter     d_tag                 NOT NULL REFERENCES t_gcal_shutter(c_tag)  ON DELETE CASCADE,

  -- How many times should the same calibration be taken
  c_gcal_step_count  int4                  NOT NULL DEFAULT 1,
  CONSTRAINT check_positive_step_count CHECK (c_gcal_step_count >= 1),

  -- These are used to filter results, not for configuring the gcal unit per se.
  c_gcal_lamp_type   e_gcal_lamp_type      NOT NULL GENERATED ALWAYS AS (CASE WHEN c_gcal_continuum IS NULL THEN 'Arc' :: e_gcal_lamp_type ELSE 'Flat' :: e_gcal_lamp_type END) STORED,
  c_gcal_baseline    e_gcal_baseline_type  NOT NULL

);