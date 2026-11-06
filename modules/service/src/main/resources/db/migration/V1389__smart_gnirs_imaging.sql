-- GNIRS imaging flats (sc-10971).  Imaging rows of the smart gcal table have
-- no grating or cross-disperser (acquisition mirror in) and are keyed by the
-- filter's central wavelength. The repeatable loader, bumped to reload the
-- definition files, now keeps the imaging rows it used to skip.

ALTER TABLE t_smart_gnirs
  ALTER COLUMN c_disperser       DROP NOT NULL,
  ALTER COLUMN c_cross_dispersed DROP NOT NULL,
  ADD CONSTRAINT t_smart_gnirs_mirror_check CHECK (
    (c_disperser IS NULL) = (c_cross_dispersed IS NULL)
  );
