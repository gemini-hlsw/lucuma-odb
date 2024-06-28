-- Drop the existing RA/Dec limits and replace them with non-nullable
-- site-specific limits.  The limits will be calculated if not explicitly
-- specified.
DROP VIEW v_cfp;

ALTER TABLE t_cfp
  DROP COLUMN c_ra_start,
  DROP COLUMN c_ra_end,
  DROP COLUMN c_dec_start,
  DROP COLUMN c_dec_end,
  ADD COLUMN c_north_ra_start  d_angle_µas NOT NULL,
  ADD COLUMN c_north_ra_end    d_angle_µas NOT NULL,
  ADD COLUMN c_north_dec_start d_angle_µas NOT NULL,
  ADD COLUMN c_north_dec_end   d_angle_µas NOT NULL,
  ADD COLUMN c_south_ra_start  d_angle_µas NOT NULL,
  ADD COLUMN c_south_ra_end    d_angle_µas NOT NULL,
  ADD COLUMN c_south_dec_start d_angle_µas NOT NULL,
  ADD COLUMN c_south_dec_end   d_angle_µas NOT NULL;

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
      format_cfp_title(c.c_type, c.c_semester, c.c_active, cte_instrument.c_instrument_names) AS c_title
    FROM
      t_cfp c
    LEFT JOIN
      cte_instrument ON c.c_cfp_id = cte_instrument.c_cfp_id
    LEFT JOIN
      cte_partner ON c.c_cfp_id = cte_partner.c_cfp_id;