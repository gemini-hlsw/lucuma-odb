-- We'll move these fields to t_dataset
ALTER TABLE t_dataset_event
  DROP COLUMN c_file_site,
  DROP COLUMN c_file_date,
  DROP COLUMN c_file_index;

-- QA States
CREATE TYPE e_dataset_qa_state As ENUM ('pass', 'usable', 'fail');
COMMENT ON TYPE e_dataset_qa_state is 'Dataset QA States.';

-- Function used to generate a dataset filename from the site, date, and index.
CREATE OR REPLACE FUNCTION generate_dataset_filename(site e_site, ldate date, idx int2)
RETURNS varchar AS $$
DECLARE
  site_prefix    varchar;
  formatted_date varchar;
  padded_idx     varchar;

BEGIN

  -- Determine the site prefix based on the site enum
  IF site = 'gn' THEN
    site_prefix := 'N';
  ELSEIF site = 'gs' THEN
    site_prefix := 'S';
  ELSE
    RAISE EXCEPTION 'Invalid site: %', site;
  END IF;

  -- Format the date column as 'YYYY-MM-DD'
  formatted_date := to_char(ldate, 'YYYYMMDD');

  -- Format the index_value with leading zeros to make it at least 4 characters long
  padded_idx := lpad(idx::varchar, 4, '0');

  RETURN site_prefix || formatted_date || 'S' || padded_idx;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

-- Dataset table.
CREATE TABLE t_dataset (

  -- Id fields
  c_step_id    d_step_id  NOT NULL REFERENCES t_step_record(c_step_id) ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED,
  c_index      int2       NOT NULL CHECK (c_index >= 0),
  PRIMARY KEY (c_step_id, c_index),

  -- Filename fields
  c_file_site  e_site     NOT NULL,
  c_file_date  date       NOT NULL CHECK (EXTRACT(YEAR FROM c_file_date) BETWEEN 0 AND 9999),
  c_file_index int4       NOT NULL CHECK (c_file_index >= 1),

  -- Filename is generated from the site, local date, and file index
  c_filename   varchar    NOT NULL GENERATED ALWAYS AS (
    generate_dataset_filename(c_file_site, c_file_date, c_file_index)
  ) STORED,

  -- Dataset QA State
  c_qa_state   e_dataset_qa_state NULL,

  -- Dataset Timestamp
  c_timestamp  timestamp  NOT NULL
);
COMMENT ON TABLE t_dataset IS 'Datasets.';