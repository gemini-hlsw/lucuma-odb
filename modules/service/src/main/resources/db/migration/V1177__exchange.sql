-- The default proprietary period is a Gemini-specific concern, so it is now
-- optional in the type table; exchange call types carry no proprietary period.
ALTER TABLE t_cfp_type
  ALTER COLUMN c_proprietary DROP NOT NULL;

-- Seed the new exchange call types (name only; no proprietary period).
INSERT INTO t_cfp_type VALUES ('keck',             'Keck Exchange',               NULL);
INSERT INTO t_cfp_type VALUES ('subaru',           'Subaru Exchange',             NULL);
INSERT INTO t_cfp_type VALUES ('subaru_intensive', 'Subaru Exchange (Intensive)', NULL);

-- Corresponding science subtypes for exchange proposals.
INSERT INTO t_science_subtype VALUES ('keck',             'K', 'Keck Exchange');
INSERT INTO t_science_subtype VALUES ('subaru',           'U', 'Subaru Exchange');
INSERT INTO t_science_subtype VALUES ('subaru_intensive', 'I', 'Subaru Exchange (Intensive)');

-- Add the observatory discriminant, defaulting existing rows to 'gemini'.  The
-- observatory is fully determined by c_type, but is stored explicitly for
-- convenience and is kept consistent by the check constraints below.
ALTER TABLE t_cfp
  ADD COLUMN c_observatory e_observatory NOT NULL DEFAULT 'gemini';

ALTER TABLE t_cfp
  ALTER COLUMN c_observatory DROP DEFAULT;

-- Add exchange-specific instrument columns.  These remain null for the
-- observatories they don't apply to.  A null array means "no instrument
-- restriction".
ALTER TABLE t_cfp
  ADD COLUMN c_keck_instruments   e_keck_instrument[]   NULL,
  ADD COLUMN c_subaru_instruments e_subaru_instrument[] NULL;

-- Exchange partners that may apply for time in a Gemini CfP.  NULL for
-- non-Gemini CfPs, empty array for Gemini CfPs without exchange partners.
ALTER TABLE t_cfp
  ADD COLUMN c_exchange_partners e_exchange_partner[] NULL;

UPDATE t_cfp
   SET c_exchange_partners = '{}'
 WHERE c_observatory = 'gemini';

-- South coordinate limits: now nullable (null for non-Gemini, which use only
-- the "north" Mauna Kea limits).
ALTER TABLE t_cfp
  ALTER COLUMN c_south_ra_start  DROP NOT NULL,
  ALTER COLUMN c_south_ra_end    DROP NOT NULL,
  ALTER COLUMN c_south_dec_start DROP NOT NULL,
  ALTER COLUMN c_south_dec_end   DROP NOT NULL;

-- The proprietary period is Gemini-specific; null for exchange calls.
ALTER TABLE t_cfp
  ALTER COLUMN c_proprietary DROP NOT NULL,
  ALTER COLUMN c_proprietary DROP DEFAULT;

-- Discriminant consistency constraints.  c_type determines the observatory and
-- which observatory-specific columns must be populated.
ALTER TABLE t_cfp
  ADD CONSTRAINT t_cfp_gemini_check CHECK (
    c_observatory <> 'gemini' OR (
      c_type               NOT IN ('keck', 'subaru', 'subaru_intensive') AND
      c_south_ra_start     IS NOT NULL AND
      c_south_ra_end       IS NOT NULL AND
      c_south_dec_start    IS NOT NULL AND
      c_south_dec_end      IS NOT NULL AND
      c_proprietary        IS NOT NULL AND
      c_exchange_partners  IS NOT NULL AND
      c_keck_instruments   IS NULL     AND
      c_subaru_instruments IS NULL
    )
  ),
  ADD CONSTRAINT t_cfp_keck_check CHECK (
    c_observatory <> 'keck' OR (
      c_type               =  'keck' AND
      c_south_ra_start     IS NULL   AND
      c_south_ra_end       IS NULL   AND
      c_south_dec_start    IS NULL   AND
      c_south_dec_end      IS NULL   AND
      c_proprietary        IS NULL   AND
      c_exchange_partners  IS NULL   AND
      c_subaru_instruments IS NULL
    )
  ),
  ADD CONSTRAINT t_cfp_subaru_check CHECK (
    c_observatory <> 'subaru' OR (
      c_type              IN ('subaru', 'subaru_intensive') AND
      c_south_ra_start    IS NULL AND
      c_south_ra_end      IS NULL AND
      c_south_dec_start   IS NULL AND
      c_south_dec_end     IS NULL AND
      c_proprietary       IS NULL AND
      c_exchange_partners IS NULL AND
      c_keck_instruments  IS NULL
    )
  );

-- Recreate the title formatter and view to account for the exchange call types
-- and the derived Subaru proposal type.
DROP VIEW v_cfp;

DROP FUNCTION format_cfp_title;
CREATE FUNCTION format_cfp_title(
  cfpType       e_cfp_type,
  semester      d_semester,
  titleOverride text,
  startDate     date,
  endDate       date,
  instruments   d_tag[]
) RETURNS text AS $$
DECLARE
  instrument_list text;
  name text;
BEGIN
  IF (titleOverride IS NOT NULL) THEN
    RETURN titleOverride;
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
    -- Exchange calls are not semester-specific; use the bare name.
    WHEN cfpType IN ('keck', 'subaru', 'subaru_intensive') THEN name
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
      -- Subaru proposal type is derived from the call type.
      (CASE
         WHEN c.c_type = 'subaru_intensive' THEN 'intensive'::e_subaru_proposal_type
         WHEN c.c_type = 'subaru'           THEN 'normal'::e_subaru_proposal_type
         ELSE NULL
       END) AS c_subaru_proposal_type,
      cte_instrument.c_instruments,
      cte_partner.c_is_open,
      format_cfp_title(c.c_type, c.c_semester, c.c_title_override, c.c_active_start, c.c_active_end, cte_instrument.c_instrument_names) AS c_title,
      cte_partner.c_non_partner = 1 AS c_allows_non_partner,
      (CASE WHEN cte_partner.c_non_partner = 1 THEN cte_partner.c_us_deadline ELSE NULL END) AS c_non_partner_deadline
    FROM
      t_cfp c
    LEFT JOIN
      cte_instrument ON c.c_cfp_id = cte_instrument.c_cfp_id
    LEFT JOIN
      cte_partner ON c.c_cfp_id = cte_partner.c_cfp_id;
