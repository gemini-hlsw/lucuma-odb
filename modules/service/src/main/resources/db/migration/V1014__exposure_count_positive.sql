-- ITC api changed such that the returned count is always more than 0
-- Drop the itc cache, reset obscalc and update the constraints
TRUNCATE t_itc_result;

UPDATE t_obscalc SET
  c_obscalc_state = 'pending',
  c_img_target_id = NULL,
  c_img_exposure_time = NULL,
  c_img_exposure_count = NULL,
  c_img_wavelength = NULL,
  c_img_single_sn = NULL,
  c_img_total_sn = NULL,
  c_spec_target_id = NULL,
  c_spec_exposure_time = NULL,
  c_spec_exposure_count = NULL,
  c_spec_wavelength = NULL,
  c_spec_single_sn = NULL,
  c_spec_total_sn = NULL
WHERE c_img_exposure_count = 0 OR c_spec_exposure_count = 0;

ALTER TABLE t_obscalc DROP CONSTRAINT IF EXISTS t_obscalc_c_img_exposure_count_check;
ALTER TABLE t_obscalc DROP CONSTRAINT IF EXISTS t_obscalc_c_spec_exposure_count_check;

ALTER TABLE t_obscalc ADD CONSTRAINT t_obscalc_c_img_exposure_count_check
  CHECK (c_img_exposure_count > 0);

ALTER TABLE t_obscalc ADD CONSTRAINT t_obscalc_c_spec_exposure_count_check
  CHECK (c_spec_exposure_count > 0);
