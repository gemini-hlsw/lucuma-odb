-- AEON/Multi-facility flag (for LP and Queue)
ALTER TABLE t_proposal ADD COLUMN c_aeon_multi_facility boolean DEFAULT false NOT NULL;

-- JWST Synergy flag (for LP and Queue)
ALTER TABLE t_proposal ADD COLUMN c_jwst_synergy boolean DEFAULT false NOT NULL;

-- US Long Term flag (Queue only)
ALTER TABLE t_proposal ADD COLUMN c_us_long_term boolean DEFAULT false NOT NULL;

-- Consider for Band 3 flag (Queue only)
ALTER TABLE t_proposal ADD COLUMN c_consider_for_band_3 boolean DEFAULT NULL;

-- Update v_proposal view to include the new columns
DROP VIEW v_proposal;
CREATE VIEW v_proposal AS
  SELECT
    p.*,
    CASE WHEN p.c_science_subtype = 'classical'           THEN c_program_id END AS c_program_id_c,
    CASE WHEN p.c_science_subtype = 'demo_science'        THEN c_program_id END AS c_program_id_s,
    CASE WHEN p.c_science_subtype = 'directors_time'      THEN c_program_id END AS c_program_id_d,
    CASE WHEN p.c_science_subtype = 'fast_turnaround'     THEN c_program_id END AS c_program_id_f,
    CASE WHEN p.c_science_subtype = 'large_program'       THEN c_program_id END AS c_program_id_l,
    CASE WHEN p.c_science_subtype = 'poor_weather'        THEN c_program_id END AS c_program_id_p,
    CASE WHEN p.c_science_subtype = 'queue'               THEN c_program_id END AS c_program_id_q,
    CASE WHEN p.c_science_subtype = 'system_verification' THEN c_program_id END AS c_program_id_v
  FROM
    t_proposal p;
