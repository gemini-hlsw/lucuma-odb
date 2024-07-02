-- Drop the existing RA/Dec limits and replace them with non-nullable
-- site-specific limits.  The limits will be calculated if not explicitly
-- specified.
DROP VIEW v_cfp;

ALTER TABLE t_cfp
  ADD COLUMN c_north_ra_start  d_angle_µas,
  ADD COLUMN c_north_ra_end    d_angle_µas,
  ADD COLUMN c_north_dec_start d_angle_µas,
  ADD COLUMN c_north_dec_end   d_angle_µas,
  ADD COLUMN c_south_ra_start  d_angle_µas,
  ADD COLUMN c_south_ra_end    d_angle_µas,
  ADD COLUMN c_south_dec_start d_angle_µas,
  ADD COLUMN c_south_dec_end   d_angle_µas;

UPDATE t_cfp
   SET c_north_ra_start  = COALESCE(c_ra_start,             0),
       c_north_ra_end    = COALESCE(c_ra_end,               0),
       c_north_dec_start = COALESCE(c_dec_start, 972000000000), -- -90º
       c_north_dec_end   = COALESCE(c_dec_end,   324000000000), --  90º
       c_south_ra_start  = COALESCE(c_ra_start,             0),
       c_south_ra_end    = COALESCE(c_ra_end,               0),
       c_south_dec_start = COALESCE(c_dec_start, 972000000000), -- -90º
       c_south_dec_end   = COALESCE(c_dec_end,   324000000000); --  90º

ALTER TABLE t_cfp
  ALTER COLUMN c_north_ra_start  SET NOT NULL,
  ALTER COLUMN c_north_ra_end    SET NOT NULL,
  ALTER COLUMN c_north_dec_start SET NOT NULL,
  ALTER COLUMN c_north_dec_end   SET NOT NULL,
  ALTER COLUMN c_south_ra_start  SET NOT NULL,
  ALTER COLUMN c_south_ra_end    SET NOT NULL,
  ALTER COLUMN c_south_dec_start SET NOT NULL,
  ALTER COLUMN c_south_dec_end   SET NOT NULL,
  DROP COLUMN c_ra_start,
  DROP COLUMN c_ra_end,
  DROP COLUMN c_dec_start,
  DROP COLUMN c_dec_end;

-- Change the active period to dates rather than specific timestamps.
ALTER TABLE t_cfp
  ADD COLUMN c_active_start date,
  ADD COLUMN c_active_end   date,
  ADD CONSTRAINT active_dates_check CHECK (c_active_start < c_active_end);

UPDATE t_cfp
   SET c_active_start = lower(c_active)::date,
       c_active_end   = upper(c_active)::date;

ALTER TABLE t_cfp
  ALTER COLUMN c_active_start SET NOT NULL,
  ALTER COLUMN c_active_end   SET NOT NULL,
  DROP COLUMN c_active;


DROP FUNCTION format_cfp_title;

-- Format the title of the CFP.
-- * Nothing prevents two or more CFPs from ending up with the same title.
-- * FT calls use the month of the midpoint of the active period
CREATE FUNCTION format_cfp_title(
  cfp_type  e_cfp_type,
  semester  d_semester,
  startDate date,
  endDate   date,
  instruments d_tag[]
) RETURNS text AS $$
DECLARE
  instrument_list text;
BEGIN
  instrument_list := array_to_string(instruments, ', ');
  RETURN CASE
    WHEN cfp_type = 'demo_science'        THEN concat(semester, ' Demo Science')
    WHEN cfp_type = 'directors_time'      THEN concat(semester, ' Director''s Time')
    WHEN cfp_type = 'fast_turnaround'     THEN
      concat(
        left(semester, -1),
        ' ',
        to_char(startDate + age(endDate, startDate)/2, 'FMMonth'),
        ' ',
        'Fast Turnaround'
      )
    WHEN cfp_type = 'large_program'       THEN concat(semester, ' Large Program')
    WHEN cfp_type = 'poor_weather'        THEN concat(semester, ' Poor Weather')
    WHEN cfp_type = 'regular_semester'    THEN concat(semester, ' Regular Semester')
    WHEN cfp_type = 'system_verification' THEN
          concat(semester, nullif(' ' || instrument_list, ' '), ' System Verification')
    ELSE concat(semester, ' ', cfp_type)
  END CASE;
END;
$$ LANGUAGE plpgsql IMMUTABLE;


-- Recreate the view, removing the old ra/dec limit null handling.
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
        (COUNT(*) FILTER (WHERE p.c_deadline IS NOT NULL AND p.c_deadline > CURRENT_TIMESTAMP) > 0) AS c_is_open
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
      format_cfp_title(c.c_type, c.c_semester, c.c_active_start, c.c_active_end, cte_instrument.c_instrument_names) AS c_title
    FROM
      t_cfp c
    LEFT JOIN
      cte_instrument ON c.c_cfp_id = cte_instrument.c_cfp_id
    LEFT JOIN
      cte_partner ON c.c_cfp_id = cte_partner.c_cfp_id;