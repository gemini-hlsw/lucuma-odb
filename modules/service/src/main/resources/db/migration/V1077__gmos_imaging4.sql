ALTER TABLE t_obscalc
  ADD  COLUMN c_has_itc_result boolean NOT NULL DEFAULT false;

UPDATE t_obscalc
  SET c_has_itc_result = c_img_target_id IS NOT NULL OR c_spec_target_id IS NOT NULL;

ALTER TABLE t_obscalc
  DROP COLUMN c_img_target_id,
  DROP COLUMN c_img_exposure_time,
  DROP COLUMN c_img_exposure_count,
  DROP COLUMN c_img_wavelength,
  DROP COLUMN c_img_single_sn,
  DROP COLUMN c_img_total_sn,
  DROP COLUMN c_spec_target_id,
  DROP COLUMN c_spec_exposure_time,
  DROP COLUMN c_spec_exposure_count,
  DROP COLUMN c_spec_wavelength,
  DROP COLUMN c_spec_single_sn,
  DROP COLUMN c_spec_total_sn;

DROP TRIGGER clean_obscalc_itc_fields_trigger ON t_target;
DROP FUNCTION clean_obscalc_itc_fields;

