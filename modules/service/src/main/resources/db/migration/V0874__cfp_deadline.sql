-- Add a nullable deadline column to the Call for Proposals table.
ALTER TABLE t_cfp
  ADD COLUMN c_deadline timestamp NULL;

-- Set the deadline to something moderately reasonable for director's time or
-- poor weather.
UPDATE t_cfp
   SET c_deadline = lower(c_active)
 WHERE c_type IN ('directors_time', 'poor_weather');

-- The deadline must be set for director's time and poor weather.  It must be
-- null otherwise.
ALTER TABLE t_cfp
  ADD CONSTRAINT check_submission_deadline CHECK (
    CASE
      WHEN c_type = 'directors_time' OR c_type = 'poor_weather' THEN c_deadline IS NOT NULL
      ELSE c_deadline IS NULL
    END
  );

-- A Call For Proposals view that
-- * adds synthetic nullable ids for the nullable RA and Dec since Grackle
--   needs those
-- * repackages the instruments as an array value so Grackle can digest it
-- * computes an "is open" boolean column to determine which CFPs are currently
--   open
DROP VIEW v_cfp;
CREATE VIEW v_cfp AS
  WITH
    cte_instrument AS (
      SELECT
        c.c_cfp_id,
        array_remove(array_agg(i.c_instrument ORDER BY i.c_instrument), NULL) AS c_instruments
      FROM
        t_cfp c
      LEFT JOIN
        t_cfp_instrument i ON c.c_cfp_id = i.c_cfp_id
      GROUP BY
        c.c_cfp_id
    ),
    cte_partner AS (
      SELECT
        c.c_cfp_id,
        COUNT(*) AS c_all_count,
        COUNT(*) FILTER (WHERE p.c_deadline > CURRENT_TIMESTAMP) AS c_open_count
      FROM
        t_cfp c
      LEFT JOIN
        t_cfp_partner p ON c.c_cfp_id = p.c_cfp_id
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
      (
        CASE
          WHEN cte_partner.c_all_count = 0
          THEN (c.c_deadline IS NOT NULL AND c.c_deadline > CURRENT_TIMESTAMP)
          ELSE (cte_partner.c_open_count > 0)
        END
      ) AS c_is_open
    FROM
      t_cfp c
    LEFT JOIN
      cte_instrument ON c.c_cfp_id = cte_instrument.c_cfp_id
    LEFT JOIN
      cte_partner ON c.c_cfp_id = cte_partner.c_cfp_id;