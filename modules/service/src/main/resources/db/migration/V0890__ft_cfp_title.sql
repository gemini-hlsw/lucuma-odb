
-- Format the title of the CFP.
CREATE OR REPLACE FUNCTION format_cfp_title(
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
        to_char(date_trunc('month', startDate) - INTERVAL '2 months', 'FMMonth'),
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