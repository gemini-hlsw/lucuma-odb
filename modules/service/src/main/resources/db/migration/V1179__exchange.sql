-- The call type, its enum, and the proprietary period are Gemini-specific
-- concepts.  Rename them with a `gemini` marker to match the `keck_` and
-- `subaru_` prefixes used by the exchange columns added below.
ALTER TYPE e_cfp_type RENAME TO e_gemini_proposal_type;

ALTER TABLE t_cfp
  RENAME COLUMN c_type TO c_gemini_proposal_type;

ALTER TABLE t_cfp
  RENAME COLUMN c_proprietary TO c_gemini_proprietary;

-- The validate_cfp_update trigger function references c_type in its body, which
-- a column rename does not update.  Recreate it (and its trigger) against the
-- renamed column.
DROP TRIGGER validate_cfp_update_trigger ON t_cfp;

CREATE OR REPLACE FUNCTION validate_cfp_update()
  RETURNS TRIGGER AS $$
DECLARE
  program_ids text;
BEGIN
  IF ((NEW.c_existence            <> OLD.c_existence)            OR
      (NEW.c_semester             <> OLD.c_semester)             OR
      (NEW.c_gemini_proposal_type <> OLD.c_gemini_proposal_type)) THEN

    -- Select program ids that use this CFP, if any.
    SELECT json_agg(c_program_id) INTO program_ids
    FROM t_proposal
    WHERE c_cfp_id = NEW.c_cfp_id;

    IF program_ids IS NOT NULL THEN
      RAISE EXCEPTION 'Cannot delete this Call for Proposals, or change its type or semester, because dependent proposals reference it: %', program_ids::text
        USING ERRCODE = 'P0001',
               DETAIL = program_ids::text;
    END IF;
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER validate_cfp_update_trigger
BEFORE UPDATE OF c_existence, c_semester, c_gemini_proposal_type ON t_cfp
FOR EACH ROW
EXECUTE FUNCTION validate_cfp_update();

-- Add the observatory discriminant, defaulting existing rows to 'gemini'.
ALTER TABLE t_cfp
  ADD COLUMN c_observatory e_observatory NOT NULL DEFAULT 'gemini';

ALTER TABLE t_cfp
  ALTER COLUMN c_observatory DROP DEFAULT;

-- Add exchange-specific columns.  Instrument arrays remain null for the
-- observatories they don't apply to; a null array means "no instrument
-- restriction".
ALTER TABLE t_cfp
  ADD COLUMN c_keck_instruments     e_keck_instrument[]    NULL,
  ADD COLUMN c_subaru_instruments   e_subaru_instrument[]  NULL,
  ADD COLUMN c_subaru_proposal_type e_subaru_proposal_type NULL;

-- Exchange partners that may apply for time in a Gemini CfP.  NULL for
-- non-Gemini CfPs, empty array for Gemini CfPs without exchange partners.
ALTER TABLE t_cfp
  ADD COLUMN c_gemini_exchange_partners e_exchange_partner[] NULL;

UPDATE t_cfp
   SET c_gemini_exchange_partners = '{}'
 WHERE c_observatory = 'gemini';

-- The call type is a Gemini concept; now nullable (null for exchange).
ALTER TABLE t_cfp
  ALTER COLUMN c_gemini_proposal_type DROP NOT NULL;

-- South coordinate limits: now nullable (null for non-Gemini, which use only
-- the "north" Mauna Kea limits).
ALTER TABLE t_cfp
  ALTER COLUMN c_south_ra_start  DROP NOT NULL,
  ALTER COLUMN c_south_ra_end    DROP NOT NULL,
  ALTER COLUMN c_south_dec_start DROP NOT NULL,
  ALTER COLUMN c_south_dec_end   DROP NOT NULL;

-- The proprietary period is Gemini-specific; null for exchange calls.
ALTER TABLE t_cfp
  ALTER COLUMN c_gemini_proprietary DROP NOT NULL,
  ALTER COLUMN c_gemini_proprietary DROP DEFAULT;

-- Discriminant consistency constraints.
ALTER TABLE t_cfp
  ADD CONSTRAINT t_cfp_gemini_check CHECK (
    c_observatory <> 'gemini' OR (
      c_gemini_proposal_type     IS NOT NULL AND
      c_south_ra_start           IS NOT NULL AND
      c_south_ra_end             IS NOT NULL AND
      c_south_dec_start          IS NOT NULL AND
      c_south_dec_end            IS NOT NULL AND
      c_gemini_proprietary       IS NOT NULL AND
      c_gemini_exchange_partners IS NOT NULL AND
      c_keck_instruments         IS NULL     AND
      c_subaru_instruments       IS NULL     AND
      c_subaru_proposal_type     IS NULL
    )
  ),
  ADD CONSTRAINT t_cfp_keck_check CHECK (
    c_observatory <> 'keck' OR (
      c_gemini_proposal_type     IS NULL AND
      c_south_ra_start           IS NULL AND
      c_south_ra_end             IS NULL AND
      c_south_dec_start          IS NULL AND
      c_south_dec_end            IS NULL AND
      c_gemini_proprietary       IS NULL AND
      c_gemini_exchange_partners IS NULL AND
      c_subaru_instruments       IS NULL AND
      c_subaru_proposal_type     IS NULL
    )
  ),
  ADD CONSTRAINT t_cfp_subaru_check CHECK (
    c_observatory <> 'subaru' OR (
      c_gemini_proposal_type     IS NULL     AND
      c_south_ra_start           IS NULL     AND
      c_south_ra_end             IS NULL     AND
      c_south_dec_start          IS NULL     AND
      c_south_dec_end            IS NULL     AND
      c_gemini_proprietary       IS NULL     AND
      c_gemini_exchange_partners IS NULL     AND
      c_keck_instruments         IS NULL     AND
      c_subaru_proposal_type     IS NOT NULL
    )
  );

-- Recreate the title formatter and view to account for the observatory-specific
-- properties and the renamed/new t_cfp columns.
DROP VIEW v_cfp;

DROP FUNCTION format_cfp_title;
CREATE FUNCTION format_cfp_title(
  observatory       e_observatory,
  semester          d_semester,
  titleOverride     text,
  startDate         date,
  endDate           date,
  geminiType        e_gemini_proposal_type,
  geminiInstruments d_tag[],
  subaruType        e_subaru_proposal_type
) RETURNS text AS $$
DECLARE
  instrument_list text;
  name text;
BEGIN
  IF (titleOverride IS NOT NULL) THEN
    RETURN titleOverride;
  END IF;

  IF (observatory = 'keck') THEN
    RETURN concat(semester, ' Keck Exchange');
  ELSIF (observatory = 'subaru') THEN
    RETURN CASE
      WHEN subaruType = 'intensive' THEN concat(semester, ' Subaru Exchange (Intensive)')
      ELSE concat(semester, ' Subaru Exchange')
    END;
  END IF;

  SELECT c_name INTO name FROM t_cfp_type WHERE c_type = geminiType;
  instrument_list := array_to_string(geminiInstruments, ', ');
  RETURN CASE
    WHEN geminiType = 'fast_turnaround' THEN
      concat(
        left(semester, -1),
        ' ',
        to_char(date_trunc('month', startDate) - INTERVAL '2 months', 'FMMonth'),
        ' ',
        name
      )
    WHEN geminiType = 'system_verification' THEN
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
        MAX(CASE WHEN p.c_partner = 'us' AND (c.c_gemini_proposal_type = 'regular_semester' OR c.c_gemini_proposal_type = 'directors_time') THEN 1 ELSE 0 END) AS c_non_partner,
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
      cte_instrument.c_instruments AS c_gemini_instruments,
      cte_partner.c_is_open,
      format_cfp_title(c.c_observatory, c.c_semester, c.c_title_override, c.c_active_start, c.c_active_end, c.c_gemini_proposal_type, cte_instrument.c_instrument_names, c.c_subaru_proposal_type) AS c_title,
      cte_partner.c_non_partner = 1 AS c_gemini_allows_non_partner,
      (CASE WHEN cte_partner.c_non_partner = 1 THEN cte_partner.c_us_deadline ELSE NULL END) AS c_gemini_non_partner_deadline
    FROM
      t_cfp c
    LEFT JOIN
      cte_instrument ON c.c_cfp_id = cte_instrument.c_cfp_id
    LEFT JOIN
      cte_partner ON c.c_cfp_id = cte_partner.c_cfp_id;

-- "c_partner" now specifically means the Gemini partner, so rename it to match
-- the new c_exchange_partner column added below.
ALTER TABLE t_program_user
  RENAME COLUMN c_partner TO c_gemini_partner;

-- Store the exchange-partner affiliation alongside the Gemini partner.
-- Set only when c_partner_link = 'has_exchange_partner'.
ALTER TABLE t_program_user
  ADD COLUMN c_exchange_partner e_exchange_partner NULL;

-- Replace the two-way partner check with a three-way version that accounts for
-- the exchange partner: exactly the column matching the link type is populated.
ALTER TABLE t_program_user
  DROP CONSTRAINT program_user_partner_check,
  ADD CONSTRAINT program_user_partner_check CHECK (
    CASE c_partner_link
      WHEN 'has_gemini_partner'   THEN c_gemini_partner IS NOT NULL AND c_exchange_partner IS NULL
      WHEN 'has_exchange_partner' THEN c_exchange_partner IS NOT NULL AND c_gemini_partner IS NULL
      ELSE                             c_gemini_partner IS NULL     AND c_exchange_partner IS NULL
    END
  );

-- Recreate v_program_user so its `p.*` picks up the new c_exchange_partner column.
DROP VIEW v_program_user;

CREATE VIEW v_program_user AS
SELECT
  p.*,
  COALESCE(p.c_preferred_email, u.c_orcid_email) AS c_email,
  COALESCE(
    user_profile_display_name(p.c_preferred_credit_name, p.c_preferred_given_name, p.c_preferred_family_name),
    user_profile_display_name(u.c_orcid_credit_name, u.c_orcid_given_name, u.c_orcid_family_name)
  ) AS c_display_name
FROM t_program_user p
LEFT JOIN t_user u ON p.c_user_id = u.c_user_id;