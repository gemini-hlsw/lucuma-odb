
-- Add `c_description` column to `t_program`.
ALTER TABLE t_program
  ADD COLUMN c_description TEXT;

-- Update `c_name` and `c_description` in `t_program` based on `t_proposal`.
UPDATE t_program m
   SET c_name        = COALESCE(s.c_title, m.c_name),
       c_description = s.c_abstract
  FROM t_proposal s
 WHERE m.c_program_id = s.c_program_id;

-- Drop `c_title` and `c_abstract` columns from `t_proposal`.
DROP VIEW v_proposal;

ALTER TABLE t_proposal
  DROP COLUMN c_title,
  DROP COLUMN c_abstract;

-- Recreate the proposal view without title and abstract.
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
    CASE WHEN p.c_science_subtype = 'system_verification' THEN c_program_id END AS c_program_id_v,
    CASE WHEN p.c_science_subtype = 'fast_turnaround' THEN
      (SELECT
        CASE WHEN COUNT(*) = 1 THEN MIN(ps.c_partner::d_tag) ELSE NULL::d_tag END
        FROM t_partner_split ps
       WHERE ps.c_program_id = p.c_program_id)
    END AS c_ft_partner
  FROM
    t_proposal p;