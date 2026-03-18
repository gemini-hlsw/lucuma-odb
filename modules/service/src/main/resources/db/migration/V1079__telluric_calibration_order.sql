-- Add calibration order to telluric resolution table

CREATE TYPE e_telluric_calibration_order AS ENUM ('before', 'after');

ALTER TABLE t_telluric_resolution
ADD COLUMN c_calibration_order e_telluric_calibration_order NOT NULL DEFAULT 'after';
