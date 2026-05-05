--- GNIRS Prism
CREATE TABLE t_gnirs_prism (
  c_tag        d_tag   NOT NULL PRIMARY KEY,
  c_short_name varchar NOT NULL,
  c_long_name  varchar NOT NULL
);

INSERT INTO t_gnirs_prism VALUES ('Mirror', 'Mirror',   'Mirror');
INSERT INTO t_gnirs_prism VALUES ('Sxd',    'Short XD', 'Short cross dispersion');
INSERT INTO t_gnirs_prism VALUES ('Lxd',    'Long XD',  'Long cross dispersion');

--- GNIRS Camera
CREATE TABLE t_gnirs_camera (
  c_tag        d_tag   NOT NULL PRIMARY KEY,
  c_short_name varchar NOT NULL,
  c_long_name  varchar NOT NULL
);

INSERT INTO t_gnirs_camera VALUES ('LongBlue',  'Long blue',  'Long blue camera');
INSERT INTO t_gnirs_camera VALUES ('LongRed',   'Long red',   'Long red camera');
INSERT INTO t_gnirs_camera VALUES ('ShortBlue', 'Short blue', 'Short blue camera');
INSERT INTO t_gnirs_camera VALUES ('ShortRed',  'Short red',  'Short red camera');

DELETE FROM t_spectroscopy_config_option_gnirs; -- Remove existing rows to avoid violating the new NOT NULL constraints.

--- Add columns to t_spectroscopy_config_option_gnirs
ALTER TABLE t_spectroscopy_config_option_gnirs
  ADD COLUMN c_prism  d_tag NOT NULL REFERENCES t_gnirs_prism(c_tag),
  ADD COLUMN c_camera d_tag NOT NULL REFERENCES t_gnirs_camera(c_tag);
