-- Smart GCAL: persist the per-calibration coadds for GNIRS.
ALTER TABLE t_smart_gnirs
  ADD COLUMN c_coadds int4 NOT NULL DEFAULT 1,
  ADD CONSTRAINT check_positive_coadds CHECK (c_coadds > 0);
