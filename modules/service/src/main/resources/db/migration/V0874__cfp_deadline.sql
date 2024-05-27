
-- Remove from the partner table all things that are not actual Gemini partners.
-- We were mixing partners with time accounting categories.
DELETE FROM t_cfp_partner
  WHERE c_partner IN ('cfh', 'keck', 'lp', 'subaru', 'gt');

DELETE FROM t_partner
  WHERE c_tag IN ('cfh', 'keck', 'lp', 'subaru', 'gt');

-- Add a nullable default submission deadline column to the Call for Proposals
-- table.
ALTER TABLE t_cfp
  ADD COLUMN c_deadline_default timestamp NULL;

-- Explicit partner deadlines are now optional.
ALTER TABLE t_cfp_partner
  ALTER COLUMN c_deadline DROP NOT NULL;

-- The explicit partner deadline overrides the default deadline in t_cfp.
ALTER TABLE t_cfp_partner
  RENAME COLUMN c_deadline TO c_deadline_override;

-- Create a partner view in order to add a c_deadline column that uses the
-- override if set, but otherwise the Call for Proposals default.
CREATE VIEW v_cfp_partner AS
  SELECT
      p.*,
      CASE WHEN p.c_deadline_override IS NULL THEN c.c_deadline_default ELSE p.c_deadline_override END AS c_deadline
    FROM
      t_cfp_partner p
    INNER JOIN
      t_cfp c ON p.c_cfp_id = c.c_cfp_id;


-- A Call For Proposals view that
-- * adds synthetic nullable ids for the nullable RA and Dec since Grackle
--   needs those
-- * repackages the instruments as an array value so Grackle can digest it
-- * computes an "is open" boolean column to determine which CFPs are currently
--   open
-- * computes a title for the CfP based on the type, semester, instruments, etc.
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
      CASE WHEN c.c_ra_start  IS NOT NULL THEN c.c_cfp_id END AS c_ra_start_id,
      CASE WHEN c.c_ra_end    IS NOT NULL THEN c.c_cfp_id END AS c_ra_end_id,
      CASE WHEN c.c_dec_start IS NOT NULL THEN c.c_cfp_id END AS c_dec_start_id,
      CASE WHEN c.c_dec_end   IS NOT NULL THEN c.c_cfp_id END AS c_dec_end_id,
      cte_instrument.c_instruments,
      cte_partner.c_is_open,
      format_cfp_title(c.c_type, c.c_semester, c.c_active, cte_instrument.c_instrument_names) AS c_title
    FROM
      t_cfp c
    LEFT JOIN
      cte_instrument ON c.c_cfp_id = cte_instrument.c_cfp_id
    LEFT JOIN
      cte_partner ON c.c_cfp_id = cte_partner.c_cfp_id;