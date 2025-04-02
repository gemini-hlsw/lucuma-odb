-- Add a site and a chargeable boolean column.  A chargeable visit is one for
-- which time may be charged.  Only one visit at a time can be charged, so there
-- is only one current chargeable visit per site.
ALTER TABLE t_visit
  ADD COLUMN c_site       e_site,
  ADD COLUMN c_chargeable boolean NOT NULL DEFAULT true;

-- Set the site based on the instrument, of which we only have GMOS-N, GMOS-S,
-- and part of Flamingos2 for now.
UPDATE t_visit
   SET c_site = 'gn'
 WHERE c_instrument = 'GmosNorth';

UPDATE t_visit
   SET c_site = 'gs'
 WHERE c_site IS NULL;

ALTER TABLE t_visit
  ALTER COLUMN c_site SET NOT NULL;

CREATE INDEX i_visit_created ON t_visit (c_created);