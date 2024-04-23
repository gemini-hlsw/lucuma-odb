
-- A Call For Proposals view that
-- * adds synthetic nullable ids for the nullable RA and Dec since Grackle
--   needs those
-- * repackages the instruments as an array value so Grackle can digest it
-- * computes an "is open" boolean column to determine which CFPs are currently
--   open
CREATE OR REPLACE VIEW v_cfp AS
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

