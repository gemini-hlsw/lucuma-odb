-- A new enum value cannot be used in the transaction that adds it, so the
-- table follows in the next migration.
ALTER TYPE e_observing_mode_type ADD VALUE 'scorpio_long_slit';
