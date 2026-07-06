-- Move Gemini CfP exchange partners out of the t_cfp.c_gemini_exchange_partners
-- array and into their own table, paralleling t_gemini_cfp_partner, so each
-- exchange partner can carry a per-partner submission deadline override.

-- 1. The new per-exchange-partner table.
CREATE TABLE t_gemini_cfp_exchange_partner (
  c_cfp_id            d_cfp_id           NOT NULL REFERENCES t_cfp(c_cfp_id) ON DELETE CASCADE,
  c_exchange_partner  e_exchange_partner NOT NULL,
  c_deadline_override timestamp          NULL,
  PRIMARY KEY (c_cfp_id, c_exchange_partner)
);
COMMENT ON TABLE t_gemini_cfp_exchange_partner IS 'Exchange partner (with optional submission deadline override) for a Gemini Call for Proposals.';

-- 2. Backfill from the existing array column.
INSERT INTO t_gemini_cfp_exchange_partner (c_cfp_id, c_exchange_partner)
SELECT c_cfp_id, unnest(c_gemini_exchange_partners)
  FROM t_cfp
 WHERE c_gemini_exchange_partners IS NOT NULL;

-- 3. A view that resolves the effective deadline (override, else the call
--    default), mirroring v_gemini_cfp_partner.
CREATE VIEW v_gemini_cfp_exchange_partner AS
  SELECT
      p.*,
      CASE WHEN p.c_deadline_override IS NULL THEN c.c_deadline_default ELSE p.c_deadline_override END AS c_deadline
    FROM
      t_gemini_cfp_exchange_partner p
    INNER JOIN
      t_cfp c ON p.c_cfp_id = c.c_cfp_id;

-- 4. Drop the exchange-partner references from the observatory discriminant
--    constraints; the concept now lives in its own table.
ALTER TABLE t_cfp
  DROP CONSTRAINT t_cfp_gemini_check,
  DROP CONSTRAINT t_cfp_keck_check,
  DROP CONSTRAINT t_cfp_subaru_check;

ALTER TABLE t_cfp
  ADD CONSTRAINT t_cfp_gemini_check CHECK (
    c_observatory <> 'gemini' OR (
      c_gemini_proposal_type     IS NOT NULL AND
      c_south_ra_start           IS NOT NULL AND
      c_south_ra_end             IS NOT NULL AND
      c_south_dec_start          IS NOT NULL AND
      c_south_dec_end            IS NOT NULL AND
      c_gemini_proprietary       IS NOT NULL AND
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
      c_keck_instruments         IS NULL     AND
      c_subaru_proposal_type     IS NOT NULL
    )
  );

-- 5. Rebuild v_cfp.
--   1. Drop the now-unused c_gemini_exchange_partners aggregate column (exchange
--      partners are exposed via v_gemini_cfp_exchange_partner instead).
--   2. Include exchange-partner deadlines in the c_is_open calculation: a call is
--      open if ANY of its partners -- Gemini or exchange (Keck/Subaru) -- still
--      has a submission deadline in the future.
DROP VIEW v_cfp;

ALTER TABLE t_cfp
  DROP COLUMN c_gemini_exchange_partners;

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
        t_gemini_cfp_instrument ci ON c.c_cfp_id = ci.c_cfp_id
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
        v_gemini_cfp_partner p ON c.c_cfp_id = p.c_cfp_id
      GROUP BY
        c.c_cfp_id
    ),
    cte_exchange_partner AS (
      SELECT
        c.c_cfp_id,
        (COUNT(*) FILTER (WHERE ep.c_deadline IS NOT NULL AND ep.c_deadline > CURRENT_TIMESTAMP) > 0) AS c_is_open
      FROM
        t_cfp c
      LEFT JOIN
        v_gemini_cfp_exchange_partner ep ON c.c_cfp_id = ep.c_cfp_id
      GROUP BY
        c.c_cfp_id
    )
    SELECT
      c.*,
      (CASE WHEN c.c_observatory = 'gemini' THEN c.c_cfp_id ELSE NULL END) AS c_gemini_cfp_id,
      (CASE WHEN c.c_observatory = 'keck'   THEN c.c_cfp_id ELSE NULL END) AS c_keck_cfp_id,
      (CASE WHEN c.c_observatory = 'subaru' THEN c.c_cfp_id ELSE NULL END) AS c_subaru_cfp_id,
      cte_instrument.c_instruments AS c_gemini_instruments,
      (COALESCE(cte_partner.c_is_open, FALSE) OR COALESCE(cte_exchange_partner.c_is_open, FALSE)) AS c_is_open,
      format_cfp_title(c.c_observatory, c.c_semester, c.c_title_override, c.c_active_start, c.c_active_end, c.c_gemini_proposal_type, cte_instrument.c_instrument_names, c.c_subaru_proposal_type) AS c_title,
      cte_partner.c_non_partner = 1 AS c_gemini_allows_non_partner,
      (CASE WHEN cte_partner.c_non_partner = 1 THEN cte_partner.c_us_deadline ELSE NULL END) AS c_gemini_non_partner_deadline
    FROM
      t_cfp c
    LEFT JOIN
      cte_instrument ON c.c_cfp_id = cte_instrument.c_cfp_id
    LEFT JOIN
      cte_partner ON c.c_cfp_id = cte_partner.c_cfp_id
    LEFT JOIN
      cte_exchange_partner ON c.c_cfp_id = cte_exchange_partner.c_cfp_id;