-- Clean up and reset calibration observations for new ROI-aware logic
-- Delete all existing twilight calibration observations since they need to be regenerated
-- with proper ROI handling

-- Delete twilight calibration observations
DELETE FROM t_observation
WHERE c_calibration_role = 'twilight';

-- Update any NULL ROI values in remaining observations with CentralSpectrum default
UPDATE t_gmos_north_long_slit
SET c_roi = 'CentralSpectrum'::d_tag
WHERE c_roi IS NULL;

UPDATE t_gmos_south_long_slit
SET c_roi = 'CentralSpectrum'::d_tag
WHERE c_roi IS NULL;

UPDATE t_gmos_north_imaging
SET c_roi = 'CentralSpectrum'::d_tag
WHERE c_roi IS NULL;

UPDATE t_gmos_south_imaging
SET c_roi = 'CentralSpectrum'::d_tag
WHERE c_roi IS NULL;

-- Set default values for ROI columns so new rows get a default when not specified
ALTER TABLE t_gmos_north_long_slit
ALTER COLUMN c_roi SET DEFAULT 'CentralSpectrum'::d_tag;

ALTER TABLE t_gmos_south_long_slit
ALTER COLUMN c_roi SET DEFAULT 'CentralSpectrum'::d_tag;

ALTER TABLE t_gmos_north_imaging
ALTER COLUMN c_roi SET DEFAULT 'CentralSpectrum'::d_tag;

ALTER TABLE t_gmos_south_imaging
ALTER COLUMN c_roi SET DEFAULT 'CentralSpectrum'::d_tag;