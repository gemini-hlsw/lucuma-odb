-- Add a nullable title override column.
ALTER TABLE t_cfp
  ADD COLUMN c_title_override text NULL DEFAULT NULL;

-- Drop the v_cfp view which calls the title formatting function.
DROP VIEW v_cfp;

-- Update the title formatting function to consider the title override.
DROP FUNCTION format_cfp_title;
CREATE FUNCTION format_cfp_title(
  titleOverride text,
  cfpType       e_cfp_type,
  semester      d_semester,
  startDate     date,
  endDate       date,
  instruments   d_tag[]
) RETURNS text AS $$
DECLARE
  instrument_list text;
  name text;
BEGIN
  SELECT c_name INTO name FROM t_cfp_type WHERE c_type = cfpType;
  instrument_list := array_to_string(instruments, ', ');
  IF (titleOverride IS NOT NULL) THEN
    RETURN titleOverride;
  ELSE
    RETURN CASE
      WHEN cfpType = 'fast_turnaround' THEN
        concat(
          left(semester, -1),
          ' ',
          to_char(date_trunc('month', startDate) - INTERVAL '2 months', 'FMMonth'),
          ' ',
          name
        )
      WHEN cfpType = 'system_verification' THEN
            concat(semester, nullif(' ' || instrument_list, ' '), ' ', name)
      ELSE concat(semester, ' ', name)
    END CASE;
  END IF;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

-- Update the view to call the title formatting function with the new
-- override argument.
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
      format_cfp_title(c.c_title_override, c.c_type, c.c_semester, c.c_active_start, c.c_active_end, cte_instrument.c_instrument_names) AS c_title,
      cte_partner.c_non_partner = 1 AS c_allows_non_partner,
      (CASE WHEN cte_partner.c_non_partner = 1 THEN cte_partner.c_us_deadline ELSE NULL END) AS c_non_partner_deadline
    FROM
      t_cfp c
    LEFT JOIN
      cte_instrument ON c.c_cfp_id = cte_instrument.c_cfp_id
    LEFT JOIN
      cte_partner ON c.c_cfp_id = cte_partner.c_cfp_id;