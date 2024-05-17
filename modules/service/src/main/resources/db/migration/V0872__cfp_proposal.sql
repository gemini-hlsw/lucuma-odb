-- Add a unique constraint on the pid, science subtype combination so that we
-- can reference it in t_proposal.
ALTER TABLE t_program
  ADD CONSTRAINT t_program_unique_pid_subtype UNIQUE (c_program_id, c_science_subtype);

-- We'll deal with intensive and exchange "later"
DELETE FROM t_proposal
  WHERE c_class = 'intensive' OR
        c_class = 'exchange';

ALTER TABLE t_proposal
  DROP CONSTRAINT t_proposal_check_min_total,
  DROP CONSTRAINT t_proposal_check_total;

-- This trigger would update the science subtype based on the proposal class.
-- The proposal class is being dropped in favor of the call for proposals type.
DROP TRIGGER update_science_subtype_trigger ON t_proposal;
DROP FUNCTION update_science_subtype;

ALTER TABLE t_proposal
  DROP COLUMN c_class,
   ADD COLUMN c_science_subtype e_science_subtype,
   ADD COLUMN c_cfp_id d_cfp_id REFERENCES t_cfp(c_cfp_id);

UPDATE t_proposal AS s
   SET c_science_subtype = g.c_science_subtype
  FROM t_program g
 WHERE s.c_program_id = g.c_program_id;

ALTER TABLE t_proposal
  ADD CONSTRAINT t_proposal_pid_subtype_fkey
    FOREIGN KEY (c_program_id, c_science_subtype)
      REFERENCES t_program(c_program_id, c_science_subtype) ON UPDATE CASCADE;

-- These checks cannot be deferred because, I think, they depend on the FK
-- reference to t_program(c_science_subtype). When updating the subtype the
-- checks are performed immediately and fail because the proposal table update
-- is still pending.  We'll need to drop the checks and add them back after the
-- proposal update.

--ALTER TABLE t_proposal
--  ADD CONSTRAINT t_proposal_check_min_total CHECK (
--    (c_min_percent_total IS NULL) OR (c_science_subtype = 'large_program')
--  ),
--  ADD CONSTRAINT t_proposal_check_total CHECK (
--    (c_total_time IS NULL) OR (c_science_subtype = 'large_program')
--  ),
--  ADD CONSTRAINT t_proposal_science_subtype CHECK (
--    CASE
--      WHEN c_science_subtype = 'classical'     THEN (c_too_activation = 'none')
--      WHEN c_science_subtype = 'large_program' THEN (c_min_percent_total IS NOT NULL) AND (c_total_time IS NOT NULL)
--      WHEN c_science_subtype = 'poor_weather'  THEN (c_too_activation = 'none') AND (c_min_percent = 0)
--    END
--  );

DROP TABLE t_proposal_class;

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