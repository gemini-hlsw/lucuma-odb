-- Imaging capabilities lookup table
CREATE TABLE t_imaging_capabilities (
  c_tag        d_tag       PRIMARY KEY,
  c_short_name varchar(64) NOT NULL,
  c_long_name  varchar(64) NOT NULL
);

INSERT INTO t_imaging_capabilities VALUES
  ('speckle',    'Speckle',    'Speckle'),
  ('wide_field', 'Wide Field', 'Wide Field');

-- Add capability column to imaging config options
ALTER TABLE t_imaging_config_option
  ADD COLUMN c_capability d_tag DEFAULT NULL REFERENCES t_imaging_capabilities(c_tag);
