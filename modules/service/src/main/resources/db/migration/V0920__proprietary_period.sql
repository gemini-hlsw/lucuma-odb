-- A table defining each CFP type's properties.
CREATE TABLE t_cfp_type (
  c_type        e_cfp_type NOT NULL PRIMARY KEY,
  c_name        text       NOT NULL,
  c_proprietary int4       NOT NULL CHECK (c_proprietary >= 0)
);
COMMENT ON TABLE t_cfp_type IS 'CFP type definition.';

INSERT INTO t_cfp_type VALUES ('demo_science',        'Demo Science',         3);
INSERT INTO t_cfp_type VALUES ('directors_time',      'Director''s Time',     6);
INSERT INTO t_cfp_type VALUES ('fast_turnaround',     'Fast Turnaround',      6);
INSERT INTO t_cfp_type VALUES ('large_program',       'Large Program',       12);
INSERT INTO t_cfp_type VALUES ('poor_weather',        'Poor Weather',        12);
INSERT INTO t_cfp_type VALUES ('regular_semester',    'Regular Semester',    12);
INSERT INTO t_cfp_type VALUES ('system_verification', 'System Verification',  3);

-- Update the CFP title formatter to use the name in t_cfp_type.
CREATE OR REPLACE FUNCTION format_cfp_title(
  cfp_type  e_cfp_type,
  semester  d_semester,
  startDate date,
  endDate   date,
  instruments d_tag[]
) RETURNS text AS $$
DECLARE
  instrument_list text;
  name text;
BEGIN
  SELECT c_name INTO name FROM t_cfp_type WHERE c_type = cfp_type;
  instrument_list := array_to_string(instruments, ', ');
  RETURN CASE
    WHEN cfp_type = 'fast_turnaround'     THEN
      concat(
        left(semester, -1),
        ' ',
        to_char(date_trunc('month', startDate) - INTERVAL '2 months', 'FMMonth'),
        ' ',
        name
      )
    WHEN cfp_type = 'system_verification' THEN
          concat(semester, nullif(' ' || instrument_list, ' '), ' ', name)
    ELSE concat(semester, ' ', name)
  END CASE;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

-- Add the default proprietary period to the CFP.
ALTER TABLE t_cfp
  ADD COLUMN c_proprietary int4 NOT NULL DEFAULT 0 CHECK (c_proprietary >= 0);

-- Update the CFP view so that it includes the proprietary period.
DROP VIEW v_cfp;

CREATE VIEW v_cfp AS
  WITH
    cte_instrument AS (
      SELECT
        c.c_cfp_id,
        array_remove(array_agg(ci.c_instrument ORDER BY ci.c_instrument), NULL) AS c_instruments,
        array_remove(array_agg(i.c_long_name ORDER BY i.c_long_name), NULL) AS c_instrument_names
      FROM
        t_cfp c
      LEFT JOIN
        t_cfp_instrument ci ON c.c_cfp_id = ci.c_cfp_id
      LEFT JOIN
        t_instrument i ON i.c_tag = ci.c_instrument
      GROUP BY
        c.c_cfp_id
    ),
    cte_partner AS (
      SELECT
        c.c_cfp_id,
        (COUNT(*) FILTER (WHERE p.c_deadline IS NOT NULL AND p.c_deadline > CURRENT_TIMESTAMP) > 0) AS c_is_open,
        MAX(CASE WHEN p.c_partner = 'us' AND (c.c_type = 'regular_semester' OR c.c_type = 'directors_time') THEN 1 ELSE 0 END) AS c_non_partner,
        MAX(CASE WHEN p.c_partner = 'us' THEN p.c_deadline ELSE NULL END) AS c_us_deadline
      FROM
        t_cfp c
      LEFT JOIN
        v_cfp_partner p ON c.c_cfp_id = p.c_cfp_id
      GROUP BY
        c.c_cfp_id
    )
    SELECT
      c.*,
      cte_instrument.c_instruments,
      cte_partner.c_is_open,
      format_cfp_title(c.c_type, c.c_semester, c.c_active_start, c.c_active_end, cte_instrument.c_instrument_names) AS c_title,
      cte_partner.c_non_partner = 1 AS c_allows_non_partner,
      (CASE WHEN cte_partner.c_non_partner = 1 THEN cte_partner.c_us_deadline ELSE NULL END) AS c_non_partner_deadline
    FROM
      t_cfp c
    LEFT JOIN
      cte_instrument ON c.c_cfp_id = cte_instrument.c_cfp_id
    LEFT JOIN
      cte_partner ON c.c_cfp_id = cte_partner.c_cfp_id;

-- Initialize any existing CFPs to the default value for its type.
UPDATE t_cfp AS c
   SET c_proprietary = t.c_proprietary
  FROM t_cfp_type t
 WHERE c.c_type = t.c_type;

-- Add a proprietary period to the program itself.
ALTER TABLE t_program
  ADD COLUMN c_proprietary int4 NOT NULL DEFAULT 0 CHECK (c_proprietary >= 0);

-- Initialize any existing programs with a linked CFP to that CFP's proprietary
-- period.
UPDATE t_program AS p
   SET c_proprietary = c.c_proprietary
  FROM t_cfp c, t_proposal r
 WHERE r.c_program_id = p.c_program_id
   AND r.c_cfp_id = c.c_cfp_id;