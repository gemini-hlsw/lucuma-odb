-- A table defining each CFP type's properties.
CREATE TABLE t_cfp_type (
  c_type        e_cfp_type NOT NULL PRIMARY KEY,
  c_name        text       NOT NULL,
  c_proprietary interval   NOT NULL
);
COMMENT ON TABLE t_cfp_type IS 'CFP type definition.';

INSERT INTO t_cfp_type VALUES ('demo_science',        'Demo Science',         '3 months' );
INSERT INTO t_cfp_type VALUES ('directors_time',      'Director''s Time',     '6 months' );
INSERT INTO t_cfp_type VALUES ('fast_turnaround',     'Fast Turnaround',      '6 months' );
INSERT INTO t_cfp_type VALUES ('large_program',       'Large Program',       '12 months' );
INSERT INTO t_cfp_type VALUES ('poor_weather',        'Poor Weather',        '12 months' );
INSERT INTO t_cfp_type VALUES ('regular_semester',    'Regular Semester',    '12 months' );
INSERT INTO t_cfp_type VALUES ('system_verification', 'System Verification',  '3 months' );

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
  ADD COLUMN c_proprietary interval NOT NULL DEFAULT '0 months';

-- Initialize any existing CFPs to the default value for its type.
UPDATE t_cfp AS c
   SET c_proprietary = t.c_proprietary
  FROM t_cfp_type t
 WHERE c.c_type = t.c_type;

-- Add a proprietary period to the program itself.
ALTER TABLE t_program
  ADD COLUMN c_proprietary interval NOT NULL DEFAULT '0 months';

-- Initialize any existing programs with a linked CFP to that CFP's proprietary
-- period.
UPDATE t_program AS p
   SET c_proprietary = c.c_proprietary
  FROM t_cfp c, t_proposal r
 WHERE r.c_program_id = p.c_program_id
   AND r.c_cfp_id = c.c_cfp_id;