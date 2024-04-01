-- Call for Proposal type.  These are similar to the science subtypes, but
-- differ in the following ways:
-- * 'poor_weather' maps to science subtype 'queue'.  There is no poor-weather-
--   specific science subtype
-- * science subtype 'classical' is a specific kind of 'regular_semester'
-- * science subtype 'queue' is a specific kind of 'regular_semester' or else
--   'poor_weather'.
CREATE TYPE e_cfp_type AS ENUM (
  'demo_science',
  'directors_time',
  'fast_turnaround',
  'large_program',
  'poor_weather',
  'regular_semester',
  'system_verification'
);
COMMENT ON TYPE e_cfp_type IS 'Call for Proposals types.';

--CREATE TABLE t_cfp_science_link (
--  c_cfp_type e_cfp_type        NOT NULL,
--  c_sci_type e_science_subtype NOT NULL,
--  PRIMARY KEY (c_cfp_type, c_sci_type)
--);

--INSERT INTO t_cfp_science_link VALUES ('demo_science',        'demo_science');
--INSERT INTO t_cfp_science_link VALUES ('directors_time',      'directors_time');
--INSERT INTO t_cfp_science_link VALUES ('fast_turnaround',     'fast_turnaround');
--INSERT INTO t_cfp_science_link VALUES ('large_program',       'large_program');
--INSERT INTO t_cfp_science_link VALUES ('poor_weather',        'queue');
--INSERT INTO t_cfp_science_link VALUES ('regular_semester',    'classical');
--INSERT INTO t_cfp_science_link VALUES ('regular_semester',    'queue');
--INSERT INTO t_cfp_science_link VALUES ('system_verification', 'system_verification');

-- Whether the call is open for more proposals (independent of individual
-- partner deadlines).
CREATE TYPE e_cfp_status AS ENUM (
  'open',
  'closed'
);
COMMENT ON TYPE e_cfp_status IS 'Call for Proposals open/closed status.';

CREATE TABLE t_cfp (
  c_id           SERIAL       PRIMARY KEY,
  c_status       e_cfp_status NOT NULL,
  c_type         e_cfp_type   NOT NULL,
  c_semester     d_semester   NOT NULL,

  -- RA and Dec range limits, if any.
  c_ra_start     d_angle_µas  NULL DEFAULT NULL,
  c_ra_end       d_angle_µas  NULL DEFAULT NULL,
  c_dec_start    d_angle_µas  NULL DEFAULT NULL,
  c_dec_end      d_angle_µas  NULL DEFAULT NULL,

  -- Active period for this CFP
  c_active       tsrange      NOT NULL
);
COMMENT ON TABLE t_cfp IS 'Call for Proposals definition.';

-- Only partners with a definition in this table will be avaliable for
-- the corresponding CFP.
CREATE TABLE t_cfp_partner (
  c_cfp_id   int4  NOT NULL REFERENCES t_cfp(c_id)      ON DELETE CASCADE,
  c_partner  d_tag NOT NULL REFERENCES t_partner(c_tag) ON DELETE CASCADE,
  PRIMARY KEY (c_cfp_id, c_partner),

  c_deadline timestamp NOT NULL
);
COMMENT ON TABLE t_cfp_partner IS 'Call for Proposal partner deadline.';

CREATE EXTENSION btree_gist;

-- Simple "resource" starting point for instruments.
CREATE TABLE t_instrument_avail (
  c_id          SERIAL  PRIMARY KEY,
  c_instrument  d_tag   NOT NULL REFERENCES t_instrument(c_tag) ON DELETE CASCADE,
  c_active      tsrange NOT NULL,

  -- Prevent overlapping active periods for the same instrument
  EXCLUDE USING GIST (c_instrument WITH =, c_active WITH &&)
);
COMMENT ON TABLE t_instrument_avail IS 'Instrument availability periods.';