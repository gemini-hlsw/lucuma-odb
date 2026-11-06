-- GNIRS imaging smart flats: the acquisition mirror is in, so the rows have no
-- disperser or cross-dispersing prism.
ALTER TABLE t_smart_gnirs
  ALTER COLUMN c_disperser       DROP NOT NULL,
  ALTER COLUMN c_cross_dispersed DROP NOT NULL,
  ADD CONSTRAINT t_smart_gnirs_mirror_check
    CHECK ((c_disperser IS NULL) = (c_cross_dispersed IS NULL)),
  ADD CONSTRAINT t_smart_gnirs_imaging_fpu_check
    CHECK (c_disperser IS NOT NULL OR c_fpu_other = 'Acquisition');

-- Every GNIRS imaging sequence gains the flats, and with them a longer time
-- estimate, so the cached obscalc results are stale.
UPDATE t_obscalc c
   SET c_obscalc_state = 'pending'
  FROM t_observation o
 WHERE c.c_observation_id = o.c_observation_id
   AND o.c_observing_mode_type = 'gnirs_imaging';
