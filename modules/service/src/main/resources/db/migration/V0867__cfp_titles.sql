-- Format the title of the CFP.
-- * Nothing prevents two or more CFPs from ending up with the same title.
-- * FT calls use the month of the midpoint of the active period
CREATE FUNCTION format_cfp_title(
  cfp_type e_cfp_type,
  semester d_semester,
  active   tsrange
) RETURNS text AS $$
BEGIN
  RETURN CASE
    WHEN cfp_type = 'demo_science'        THEN concat(semester, ' ', 'Demo Science')
    WHEN cfp_type = 'directors_time'      THEN concat(semester, ' ', 'Director''s Time')
    WHEN cfp_type = 'fast_turnaround'     THEN
      concat(
        left(semester, length(semester) - 1),
        ' ',
        to_char(lower(active) + (upper(active) - lower(active))/2, 'FMMonth'),
        ' Fast Turnaround'
      )
    WHEN cfp_type = 'large_program'       THEN concat(semester, ' ', 'Large Program')
    WHEN cfp_type = 'poor_weather'        THEN concat(semester, ' ', 'Poor Weather')
    WHEN cfp_type = 'regular_semester'    THEN concat(semester, ' ', 'Regular Semester')
    WHEN cfp_type = 'system_verification' THEN concat(semester, ' ', 'System Verification')
    ELSE concat(semester, ' ', 'Unknown Call Type: ', cfp_type)
  END;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

ALTER TABLE t_cfp
  ADD COLUMN c_title text GENERATED ALWAYS AS (format_cfp_title(c_type, c_semester, c_active)) STORED;

-- Recreate the view to pick up the title column.
DROP VIEW v_cfp;

CREATE VIEW v_cfp AS
  SELECT
    c.*,
    CASE WHEN c.c_ra_start  IS NOT NULL THEN c.c_cfp_id END AS c_ra_start_id,
    CASE WHEN c.c_ra_end    IS NOT NULL THEN c.c_cfp_id END AS c_ra_end_id,
    CASE WHEN c.c_dec_start IS NOT NULL THEN c.c_cfp_id END AS c_dec_start_id,
    CASE WHEN c.c_dec_end   IS NOT NULL THEN c.c_cfp_id END AS c_dec_end_id,
    array_remove(array_agg(i.c_instrument ORDER BY i.c_instrument), NULL) AS c_instruments,
    (SELECT COUNT(*)
      FROM t_cfp_partner
     WHERE t_cfp_partner.c_cfp_id = c.c_cfp_id
       AND t_cfp_partner.c_deadline > CURRENT_TIMESTAMP
    ) > 0 AS c_is_open
  FROM
    t_cfp c
  LEFT JOIN
    t_cfp_instrument i ON c.c_cfp_id = i.c_cfp_id
  GROUP BY
    c.c_cfp_id;