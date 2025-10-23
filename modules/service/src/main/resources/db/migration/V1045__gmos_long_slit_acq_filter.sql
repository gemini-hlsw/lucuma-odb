-- Add an attribute on the filter table to keep up with whether the filter is
-- available for use in acquisition.
ALTER TABLE t_gmos_north_filter
  ADD COLUMN c_is_acquisition_filter boolean NOT NULL DEFAULT FALSE;

UPDATE t_gmos_north_filter
  SET c_is_acquisition_filter = TRUE
  WHERE c_tag IN ('GPrime', 'RPrime', 'IPrime', 'ZPrime');

-- Add a column to keep up with the explicitly specified GMOS North filter
-- if provided (this is optional and nullable).
ALTER TABLE t_gmos_north_long_slit
  ADD COLUMN c_acquisition_filter d_tag REFERENCES t_gmos_north_filter(c_tag);

-- Same setup for GMOS South.
ALTER TABLE t_gmos_south_filter
  ADD COLUMN c_is_acquisition_filter bool NOT NULL DEFAULT FALSE;

UPDATE t_gmos_south_filter
  SET c_is_acquisition_filter = TRUE
  WHERE c_tag IN ('UPrime', 'GPrime', 'RPrime', 'IPrime', 'ZPrime');

ALTER TABLE t_gmos_south_long_slit
  ADD COLUMN c_acquisition_filter d_tag REFERENCES t_gmos_south_filter(c_tag);

-- Revive the GMOS long slit views in order to provide the default filter
-- as a SqlField.  Otherwise, the GmosNorthLongSlitMapping is complicated.

CREATE VIEW v_gmos_north_long_slit AS
SELECT
  m.*,
  (
    SELECT f.c_tag
      FROM t_gmos_north_filter f
      WHERE f.c_is_acquisition_filter
      ORDER BY abs(f.c_wavelength - m.c_central_wavelength)
      LIMIT 1
  ) AS c_acquisition_filter_default
FROM t_gmos_north_long_slit m;

CREATE VIEW v_gmos_south_long_slit AS
SELECT
  m.*,
  (
    SELECT f.c_tag
      FROM t_gmos_south_filter f
      WHERE f.c_is_acquisition_filter
      ORDER BY abs(f.c_wavelength - m.c_central_wavelength)
      LIMIT 1
  ) AS c_acquisition_filter_default
FROM t_gmos_south_long_slit m;

-- Now, make sure that the explicit filter assigned for acquisition is really
-- valid.

CREATE FUNCTION check_gmos_acquisition_filter_is_valid()
RETURNS TRIGGER AS $$
DECLARE
  filter_table text := tg_argv[0];
  is_ok boolean;
BEGIN

  IF new.c_acquisition_filter IS NOT NULL THEN
    EXECUTE format(
      $sql$
        SELECT EXISTS (
          SELECT 1 FROM %I f WHERE f.c_tag = $1 AND f.c_is_acquisition_filter
        )
      $sql$, filter_table
    ) INTO is_ok USING NEW.c_acquisition_filter;

    IF NOT is_ok THEN
      RAISE EXCEPTION
        'Invalid acquisition filter % for table %, must have c_is_acquisition_filter = TRUE',
        NEW.c_acquisition_filter,
        tg_table_name
        USING errcode = 'check_violation';
    END IF;
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER check_gmos_north_acquisition_filter_is_valid_trigger
BEFORE INSERT OR UPDATE OF c_acquisition_filter
ON t_gmos_north_long_slit
FOR EACH ROW
EXECUTE FUNCTION check_gmos_acquisition_filter_is_valid('t_gmos_north_filter');

CREATE TRIGGER check_gmos_south_acquisition_filter_is_valid_trigger
BEFORE INSERT OR UPDATE OF c_acquisition_filter
ON t_gmos_south_long_slit
FOR EACH ROW
EXECUTE FUNCTION check_gmos_acquisition_filter_is_valid('t_gmos_south_filter');