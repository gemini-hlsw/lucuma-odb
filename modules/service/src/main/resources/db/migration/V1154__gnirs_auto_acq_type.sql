-- GNIRS acquisition type: make c_acq_type nullable, mirroring the read mode
-- handling in V1153. A NULL c_acq_type now means "Automatic": the acquisition
-- type is computed from the acquisition exposure time at sequence-generation
-- time (GnirsAcquisitionType.forAcquisitionExposureTime). The three concrete
-- acquisition types are stored as their enum value, exactly as before.
--
-- Dropping NOT NULL / DEFAULT does not change the column's PostgreSQL type, so
-- the v_gnirs_long_slit view (which expands ls.*) does not need to be recreated.
ALTER TABLE t_gnirs_long_slit ALTER COLUMN c_acq_type DROP NOT NULL;
ALTER TABLE t_gnirs_long_slit ALTER COLUMN c_acq_type DROP DEFAULT;
