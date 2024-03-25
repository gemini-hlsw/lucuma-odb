-- Spectroscopy Configuration Options
CREATE TABLE t_spectroscopy_config_option (
  c_instrument           d_tag           NOT NULL REFERENCES t_instrument(c_tag),
  c_index                int4            NOT NULL,

  PRIMARY KEY (c_instrument, c_index),

  c_name                 text            NOT NULL CHECK (length(c_name) > 0),
  c_focal_plane          d_tag           NOT NULL REFERENCES t_focal_plane(c_tag),

  c_fpu_label            text            NOT NULL CHECK (length(c_fpu_label) > 0),
  c_slit_width           d_angle_µas     NOT NULL,
  c_slit_length          d_angle_µas     NOT NULL,

  c_disperser_label      text            NOT NULL CHECK (length(c_disperser_label) > 0),
  c_filter_label         text                     CHECK (c_filter_label IS NULL OR length(c_filter_label) > 0),

  c_wavelength_min       d_wavelength_pm NOT NULL,
  c_wavelength_max       d_wavelength_pm NOT NULL,
  c_wavelength_optimal   d_wavelength_pm NOT NULL,
  c_wavelength_coverage  d_wavelength_pm NOT NULL,

  c_resolution           int4            NOT NULL CHECK (c_resolution > 0),

  c_ao                   boolean         NOT NULL DEFAULT false,

  c_capability           d_tag           DEFAULT NULL REFERENCES t_spectroscopy_capabilities(c_tag)

);
COMMENT ON TABLE t_spectroscopy_config_option IS 'Spectroscopy Configuration Option';


CREATE OR REPLACE FUNCTION create_spectroscopy_config_option_temp_table(
  temp_table_name    TEXT,
  instrument_columns TEXT[]
)
RETURNS VOID AS $$
DECLARE
  common_columns_sql     TEXT;
  instrument_columns_sql TEXT;
  create_table_sql       TEXT;
BEGIN
  -- All temporary tables will have the columns from t_spectroscopy_config_option
  SELECT string_agg(column_name || ' ' || data_type, ', ' ORDER BY ordinal_position)
  INTO common_columns_sql
  FROM information_schema.columns
  WHERE table_name = 't_spectroscopy_config_option';

  -- followed by instrument-specific columns
  instrument_columns_sql := array_to_string(instrument_columns, ', ');

  -- Generate CREATE TABLE statement
  create_table_sql := 'CREATE TEMPORARY TABLE ' || temp_table_name || ' (' ||
                        common_columns_sql      || ', ' ||
                        instrument_columns_sql  ||
                      ');';

  RAISE NOTICE '%', create_table_sql;

  EXECUTE create_table_sql;
END;
$$ LANGUAGE plpgsql;



CREATE OR REPLACE FUNCTION insert_into_spectroscopy_config_option(
  temp_table_name TEXT
)
RETURNS VOID AS $$
DECLARE
  common_columns_sql TEXT;
  insert_sql TEXT;
BEGIN
  -- Get the column names of interest.
  SELECT string_agg(column_name, ', ' ORDER BY ordinal_position)
  INTO common_columns_sql
  FROM information_schema.columns
  WHERE table_name = 't_spectroscopy_config_option';

  -- Make the INSERT INTO statement 
  insert_sql := 'INSERT INTO t_spectroscopy_config_option (' ||
                  common_columns_sql ||
                ') SELECT '          ||
                  common_columns_sql ||
                ' FROM '             ||
                  temp_table_name    ||
                ';';


  EXECUTE insert_sql;
END;
$$ LANGUAGE plpgsql;

