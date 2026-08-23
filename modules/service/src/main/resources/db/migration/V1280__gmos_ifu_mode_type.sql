-- Add the GMOS North/South IFU observing mode types to the e_observing_mode_type
-- enum.  ALTER TYPE ... ADD VALUE cannot share a transaction with any use of the
-- new value, so the tables that reference these live in the next migration.
ALTER TYPE e_observing_mode_type ADD VALUE 'gmos_north_ifu';
ALTER TYPE e_observing_mode_type ADD VALUE 'gmos_south_ifu';
