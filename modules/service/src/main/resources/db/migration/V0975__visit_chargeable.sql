-- Add a chargeable boolean column to visit.
ALTER TABLE t_visit
  ADD COLUMN c_site       e_site,
  ADD COLUMN c_chargeable boolean NOT NULL DEFAULT true;

UPDATE t_visit
   SET c_site = gn
 WHERE c_instrument = 'GmosNorth';

UPDATE t_visit
   SET c_site = gn
 WHERE c_site IS NULL;

ALTER TABLE t_visit
  ALTER COLUMN c_site SET NOT NULL;

CREATE INDEX i_visit_created ON t_visit (c_created);