-- Call for proposal GIDs, e.g. c-123
CREATE DOMAIN d_cfp_id AS varchar
  CHECK (VALUE ~ '^c-[0-9a-f]{3,}$');
COMMENT ON DOMAIN d_cfp_id IS 'Call for Proposals ID';

CREATE SEQUENCE s_cfp_id START 256; -- three hex digits

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

-- Whether the call is open for more proposals (independent of individual
-- partner deadlines).
CREATE TYPE e_cfp_status AS ENUM (
  'open',
  'closed'
);
COMMENT ON TYPE e_cfp_status IS 'Call for Proposals open/closed status.';

CREATE TABLE t_cfp (
  c_cfp_id       d_cfp_id     PRIMARY KEY DEFAULT 'c-' || to_hex(nextval('s_cfp_id')),
  c_status       e_cfp_status NOT NULL,
  c_type         e_cfp_type   NOT NULL,
  c_semester     d_semester   NOT NULL,

  -- RA and Dec range limits, if any.
  c_ra_start     d_angle_µas  NULL DEFAULT NULL,
  c_ra_end       d_angle_µas  NULL DEFAULT NULL,

  CONSTRAINT both_ra_limits_or_neither CHECK (
    (
      c_ra_start IS NULL AND c_ra_end IS NULL
    ) OR (
      c_ra_start IS NOT NULL AND c_ra_end IS NOT NULL
    )
  ),

  c_dec_start    d_angle_µas  NULL DEFAULT NULL,
  c_dec_end      d_angle_µas  NULL DEFAULT NULL,

  CONSTRAINT both_dec_limits_or_neither CHECK (
    (
      c_dec_start IS NULL AND c_dec_end IS NULL
    ) OR (
      c_dec_start IS NOT NULL AND c_dec_end IS NOT NULL
    )
  ),

  -- Active period for this CFP
  c_active       tsrange      NOT NULL,

  c_existence    e_existence  NOT NULL DEFAULT 'present'
);
COMMENT ON TABLE t_cfp IS 'Call for Proposals definition.';

-- Only partners with a definition in this table will be avaliable for
-- the corresponding CFP.
CREATE TABLE t_cfp_partner (
  c_cfp_id   d_cfp_id NOT NULL REFERENCES t_cfp(c_cfp_id)  ON DELETE CASCADE,
  c_partner  d_tag    NOT NULL REFERENCES t_partner(c_tag) ON DELETE CASCADE,
  PRIMARY KEY (c_cfp_id, c_partner),

  c_deadline timestamp NOT NULL
);
COMMENT ON TABLE t_cfp_partner IS 'Call for Proposal partner deadline.';

-- Tracks the instruments, if any, to which the CfP is limited.
CREATE TABLE t_cfp_instrument (
  c_cfp_id     d_cfp_id NOT NULL REFERENCES t_cfp(c_cfp_id)     ON DELETE CASCADE,
  c_instrument d_tag    NOT NULL REFERENCES t_instrument(c_tag) ON DELETE CASCADE,
  PRIMARY KEY (c_cfp_id, c_instrument)
);
COMMENT ON TABLE t_cfp_instrument IS 'Call for Proposal instruments.';

-- A view to repackage the instruments as an array value so Grackle can digest
-- it. Also, adds synthetic nullable ids for the nullable RA and Dec since
-- Grackle needs those as well.
CREATE VIEW v_cfp AS
  SELECT
    c.*,
    CASE WHEN c.c_ra_start  IS NOT NULL THEN c.c_cfp_id END AS c_ra_start_id,
    CASE WHEN c.c_ra_end    IS NOT NULL THEN c.c_cfp_id END AS c_ra_end_id,
    CASE WHEN c.c_dec_start IS NOT NULL THEN c.c_cfp_id END AS c_dec_start_id,
    CASE WHEN c.c_dec_end   IS NOT NULL THEN c.c_cfp_id END AS c_dec_end_id,
    array_remove(array_agg(i.c_instrument ORDER BY i.c_instrument), NULL) AS c_instruments
  FROM
    t_cfp c
  LEFT JOIN
    t_cfp_instrument i ON c.c_cfp_id = i.c_cfp_id
  GROUP BY
    c.c_cfp_id;