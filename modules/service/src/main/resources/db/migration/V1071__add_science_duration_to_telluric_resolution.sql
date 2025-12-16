-- Store total program time for telluric resolution
TRUNCATE t_telluric_resolution;

ALTER TABLE t_telluric_resolution
ADD COLUMN c_science_duration interval NOT NULL;
