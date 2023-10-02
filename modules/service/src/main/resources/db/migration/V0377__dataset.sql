-- We'll move these fields to t_dataset
ALTER TABLE t_dataset_event
  DROP COLUMN c_file_site,
  DROP COLUMN c_file_date,
  DROP COLUMN c_file_index;

-- QA States
CREATE TYPE e_dataset_qa_state As ENUM ('Pass', 'Usable', 'Fail');
COMMENT ON TYPE e_dataset_qa_state is 'Dataset QA States.';

-- Function used to generate a dataset filename from the site, date, and index.
CREATE OR REPLACE FUNCTION generate_dataset_filename(site e_site, ldate date, idx int4)
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

  RETURN site_prefix || formatted_date || 'S' || padded_idx || '.fits';
END;
$$ LANGUAGE plpgsql IMMUTABLE;

-- Dataset table.
CREATE TABLE t_dataset (

  -- Id fields
  c_step_id    d_step_id  NOT NULL REFERENCES t_step_record(c_step_id) ON DELETE CASCADE,
  c_index      int2       NOT NULL CHECK (c_index >= 0),
  PRIMARY KEY (c_step_id, c_index),

  -- Filename fields
  c_file_site  e_site     NOT NULL,
  c_file_date  date       NOT NULL CHECK (EXTRACT(YEAR FROM c_file_date) BETWEEN 0 AND 9999),
  c_file_index int4       NOT NULL CHECK (c_file_index >= 1),

  UNIQUE (c_file_site, c_file_date, c_file_index),

  -- Filename is generated from the site, local date, and file index
  c_filename   varchar    NOT NULL GENERATED ALWAYS AS (
    generate_dataset_filename(c_file_site, c_file_date, c_file_index)
  ) STORED,

  -- Dataset QA State
  c_qa_state   e_dataset_qa_state NULL,

  -- Dataset Timestamps
  c_start_time timestamp  NULL,
  c_end_time   timestamp  NULL,

  -- If end is defined, then start must be defined.  Start must come before end.
  CONSTRAINT t_dataset_check_times CHECK (
    (c_end_time IS NULL) OR
    (c_start_time IS NOT NULL AND (c_start_time <= c_end_time))
  )
);
COMMENT ON TABLE t_dataset IS 'Datasets.';

-- Sets the dataset id index from the max index associated with the step id + 1
CREATE OR REPLACE FUNCTION set_dataset_index()
RETURNS TRIGGER AS $$
BEGIN
  NEW.c_index := COALESCE((SELECT MAX(c_index) FROM t_dataset WHERE c_step_id = NEW.c_step_id), 0) + 1;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER set_dataset_index_trigger
BEFORE INSERT ON t_dataset
FOR EACH ROW EXECUTE FUNCTION set_dataset_index();
