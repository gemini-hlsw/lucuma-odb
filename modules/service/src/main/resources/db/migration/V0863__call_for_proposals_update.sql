DROP VIEW v_cfp;

ALTER TABLE t_cfp
  ADD c_active_start timestamp,
  ADD c_active_end   timestamp;

UPDATE t_cfp
  SET c_active_start = COALESCE(lower(c_active), '2000-01-01 00:00:00'::timestamp),
      c_active_end   = COALESCE(upper(c_active), '2000-01-01 00:00:00'::timestamp);

ALTER TABLE t_cfp
  DROP c_active,
  ALTER COLUMN c_active_start SET NOT NULL,
  ALTER COLUMN c_active_end   SET NOT NULL,
  ADD CONSTRAINT active_start_before_end CHECK (c_active_start <= c_active_end);

-- A view to:
-- * repackage the instruments as an array value so Grackle can digest it
-- * add synthetic nullable ids for the nullable RA and Dec since Grackle needs those as well
CREATE VIEW v_cfp AS
  SELECT
    c.*,
    CASE WHEN c.c_ra_start  IS NOT NULL THEN c.c_cfp_id END AS c_ra_start_id,
    CASE WHEN c.c_ra_end    IS NOT NULL THEN c.c_cfp_id END AS c_ra_end_id,
    CASE WHEN c.c_dec_start IS NOT NULL THEN c.c_cfp_id END AS c_dec_start_id,
    CASE WHEN c.c_dec_end   IS NOT NULL THEN c.c_cfp_id END AS c_dec_end_id,
    array_remove(array_agg(i.c_instrument ORDER BY i.c_instrument), NULL) AS c_instruments
  FROM
    t_cfp c
  LEFT JOIN
    t_cfp_instrument i ON c.c_cfp_id = i.c_cfp_id
  GROUP BY
    c.c_cfp_id;