-- Add the observatory discriminant, defaulting existing rows to 'gemini'
ALTER TABLE t_cfp
  ADD COLUMN c_observatory e_observatory NOT NULL DEFAULT 'gemini';

-- Remove the default now that existing rows are set
ALTER TABLE t_cfp
  ALTER COLUMN c_observatory DROP DEFAULT;

-- Add exchange-specific columns
ALTER TABLE t_cfp
  ADD COLUMN c_keck_instruments     e_keck_instrument[]    NULL,
  ADD COLUMN c_subaru_instruments   e_subaru_instrument[]  NULL,
  ADD COLUMN c_subaru_proposal_type e_subaru_proposal_type NULL;

-- Exchange partners that may apply for time in a Gemini CfP.  NULL for
-- non-Gemini CfPs, empty array (the default) for Gemini CfPs without
-- exchange partners.
ALTER TABLE t_cfp
  ADD COLUMN c_exchange_partners e_exchange_partner[] NULL;

UPDATE t_cfp
   SET c_exchange_partners = '{}'
 WHERE c_observatory = 'gemini';

-- c_type: now nullable (null for non-Gemini)
ALTER TABLE t_cfp
  ALTER COLUMN c_type DROP NOT NULL;

-- South coordinate limits: now nullable (null for non-Gemini)
ALTER TABLE t_cfp
  ALTER COLUMN c_south_ra_start  DROP NOT NULL,
  ALTER COLUMN c_south_ra_end    DROP NOT NULL,
  ALTER COLUMN c_south_dec_start DROP NOT NULL,
  ALTER COLUMN c_south_dec_end   DROP NOT NULL;

-- c_proprietary: now nullable (Gemini only)
ALTER TABLE t_cfp
  ALTER COLUMN c_proprietary DROP NOT NULL,
  ALTER COLUMN c_proprietary DROP DEFAULT;

-- Discriminant consistency constraints
ALTER TABLE t_cfp
  ADD CONSTRAINT t_cfp_gemini_check CHECK (
    c_observatory <> 'gemini' OR (
      c_type                 IS NOT NULL AND
      c_south_ra_start       IS NOT NULL AND
      c_south_ra_end         IS NOT NULL AND
      c_south_dec_start      IS NOT NULL AND
      c_south_dec_end        IS NOT NULL AND
      c_proprietary          IS NOT NULL AND
      c_exchange_partners    IS NOT NULL AND
      c_keck_instruments     IS NULL     AND
      c_subaru_instruments   IS NULL     AND
      c_subaru_proposal_type IS NULL
    )
  ),
  ADD CONSTRAINT t_cfp_keck_check CHECK (
    c_observatory <> 'keck' OR (
      c_type                 IS NULL AND
      c_south_ra_start       IS NULL AND
      c_south_ra_end         IS NULL AND
      c_south_dec_start      IS NULL AND
      c_south_dec_end        IS NULL AND
      c_proprietary          IS NULL AND
      c_exchange_partners    IS NULL AND
      c_subaru_instruments   IS NULL AND
      c_subaru_proposal_type IS NULL
    )
  ),
  ADD CONSTRAINT t_cfp_subaru_check CHECK (
    c_observatory <> 'subaru' OR (
      c_type                 IS NULL     AND
      c_south_ra_start       IS NULL     AND
      c_south_ra_end         IS NULL     AND
      c_south_dec_start      IS NULL     AND
      c_south_dec_end        IS NULL     AND
      c_proprietary          IS NULL     AND
      c_exchange_partners    IS NULL     AND
      c_keck_instruments     IS NULL     AND
      c_subaru_proposal_type IS NOT NULL
    )
  );

-- Recreate the view and title formatter so they account for the
-- observatory-specific properties and the new t_cfp columns.
DROP VIEW v_cfp;

DROP FUNCTION format_cfp_title;
CREATE FUNCTION format_cfp_title(
  observatory   e_observatory,
  cfpType       e_cfp_type,
  semester      d_semester,
  titleOverride text,
  startDate     date,
  endDate       date,
  instruments   d_tag[],
  subaruType    e_subaru_proposal_type
) RETURNS text AS $$
DECLARE
  instrument_list text;
  name text;
BEGIN
  IF (titleOverride IS NOT NULL) THEN
    RETURN titleOverride;
  END IF;

  IF (observatory = 'keck') THEN
    RETURN 'Keck Exchange';
  ELSIF (observatory = 'subaru') THEN
    RETURN CASE
      WHEN subaruType = 'intensive' THEN 'Subaru Exchange (Intensive)'
      ELSE 'Subaru Exchange'
    END;
  END IF;

  SELECT c_name INTO name FROM t_cfp_type WHERE c_type = cfpType;
  instrument_list := array_to_string(instruments, ', ');
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
END;
$$ LANGUAGE plpgsql IMMUTABLE;

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
      (CASE WHEN c.c_observatory = 'gemini' THEN c.c_cfp_id ELSE NULL END) AS c_gemini_cfp_id,
      (CASE WHEN c.c_observatory = 'keck'   THEN c.c_cfp_id ELSE NULL END) AS c_keck_cfp_id,
      (CASE WHEN c.c_observatory = 'subaru' THEN c.c_cfp_id ELSE NULL END) AS c_subaru_cfp_id,
      cte_instrument.c_instruments,
      cte_partner.c_is_open,
      format_cfp_title(c.c_observatory, c.c_type, c.c_semester, c.c_title_override, c.c_active_start, c.c_active_end, cte_instrument.c_instrument_names, c.c_subaru_proposal_type) AS c_title,
      cte_partner.c_non_partner = 1 AS c_allows_non_partner,
      (CASE WHEN cte_partner.c_non_partner = 1 THEN cte_partner.c_us_deadline ELSE NULL END) AS c_non_partner_deadline
    FROM
      t_cfp c
    LEFT JOIN
      cte_instrument ON c.c_cfp_id = cte_instrument.c_cfp_id
    LEFT JOIN
      cte_partner ON c.c_cfp_id = cte_partner.c_cfp_id;
