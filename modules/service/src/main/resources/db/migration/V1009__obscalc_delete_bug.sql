-- Change the t_obscalc target FK reference constraints to NOT cascade on
-- deletion.  We don't want to remove the entire row, just invalidate the
-- calculation resuls.  Note, t_target entries are deleted for real by the
-- calibrations service.
--
-- See line 234 in V0988_obscalc.sql.

ALTER TABLE t_obscalc DROP CONSTRAINT t_obscalc_c_img_target_id_fkey;

ALTER TABLE t_obscalc
ADD CONSTRAINT t_obscalc_c_img_target_id_fkey
FOREIGN KEY (c_img_target_id) REFERENCES t_target(c_target_id)
ON DELETE SET NULL;

ALTER TABLE t_obscalc DROP CONSTRAINT t_obscalc_c_spec_target_id_fkey;

ALTER TABLE t_obscalc
ADD CONSTRAINT t_obscalc_c_spec_target_id_fkey
FOREIGN KEY (c_spec_target_id) REFERENCES t_target(c_target_id)
ON DELETE SET NULL;

CREATE OR REPLACE FUNCTION clean_obscalc_itc_fields()
RETURNS TRIGGER AS $$
DECLARE
  obs_id d_observation_id;
BEGIN
  FOR obs_id IN
    SELECT c_observation_id
    FROM t_obscalc
    WHERE c_img_target_id  = OLD.c_target_id
       OR c_spec_target_id = OLD.c_target_id
  LOOP
    -- Clean the obscalc fields
    UPDATE t_obscalc
    SET
      c_img_target_id       = NULL,
      c_img_exposure_time   = NULL,
      c_img_exposure_count  = NULL,
      c_img_wavelength      = NULL,
      c_img_single_sn       = NULL,
      c_img_total_sn        = NULL,
      c_spec_target_id      = NULL,
      c_spec_exposure_time  = NULL,
      c_spec_exposure_count = NULL,
      c_spec_wavelength     = NULL,
      c_spec_single_sn      = NULL,
      c_spec_total_sn       = NULL
    WHERE c_observation_id = obs_id;

    CALL invalidate_obscalc(obs_id);
  END LOOP;

  RETURN OLD;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER clean_obscalc_itc_fields_trigger
  BEFORE DELETE ON t_target
  FOR EACH ROW
  EXECUTE FUNCTION clean_obscalc_itc_fields();