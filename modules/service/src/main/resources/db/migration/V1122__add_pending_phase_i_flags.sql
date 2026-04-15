-- AEON/Multi-facility flag (for LP and Queue)
ALTER TABLE t_proposal ADD COLUMN c_aeon_multi_facility boolean DEFAULT false NOT NULL;

-- JWST Synergy flag (for LP and Queue)
ALTER TABLE t_proposal ADD COLUMN c_jwst_synergy boolean DEFAULT false NOT NULL;

-- US Long Term flag (Queue only)
ALTER TABLE t_proposal ADD COLUMN c_us_long_term boolean DEFAULT false NOT NULL;

-- Consider for Band 3 flag (Queue only)
ALTER TABLE t_proposal ADD COLUMN c_consider_for_band_3 boolean DEFAULT NULL;
