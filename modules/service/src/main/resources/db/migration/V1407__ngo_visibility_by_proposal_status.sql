-- Redefines program_ngo_partners (V1370), the single definition of which
-- programs an NGO user may read, to depend on the proposal status.  An NGO user
-- may read a program when their partner:
--
-- * holds time in it: a non-zero allocation in the partner's time accounting
--   category (partner tags and their category tags coincide; categories such as
--   'dd' are not partners), unless the proposal was not accepted; or
-- * is asked for time while the proposal is not yet decided (not submitted or
--   submitted): a non-zero split on an existing proposal (Explore stores a 0%
--   row for every partner, and deleteProposal leaves the t_partner_split rows
--   behind), or, for a Fast Turnaround or Poor Weather proposal, which has no
--   splits, the PI's Gemini partner.
--
-- No NGO may read a program whose proposal was not accepted.  A program without
-- a proposal has status 'not_submitted', so its allocations still count.
CREATE OR REPLACE FUNCTION program_ngo_partners(pid d_program_id) RETURNS SETOF d_tag
LANGUAGE sql STABLE AS $$
  WITH prog AS (
    SELECT c_proposal_status AS status FROM t_program WHERE c_program_id = pid
  )
  SELECT s.c_partner
    FROM t_partner_split s, prog
   WHERE s.c_program_id = pid
     AND s.c_percent > 0
     AND prog.status IN ('not_submitted', 'submitted')
     AND EXISTS (SELECT 1 FROM t_proposal pr WHERE pr.c_program_id = pid)
  UNION
  SELECT u.c_gemini_partner
    FROM t_program_user u
    JOIN t_proposal pr ON pr.c_program_id = u.c_program_id
    JOIN t_cfp c       ON c.c_cfp_id = pr.c_cfp_id, prog
   WHERE u.c_program_id = pid
     AND u.c_role = 'pi'
     AND u.c_gemini_partner IS NOT NULL
     AND c.c_gemini_proposal_type IN ('fast_turnaround', 'poor_weather')
     AND prog.status IN ('not_submitted', 'submitted')
  UNION
  SELECT a.c_ta_category
    FROM t_allocation a, prog
   WHERE a.c_program_id = pid
     AND a.c_duration > 'PT'
     AND a.c_ta_category IN (SELECT c_tag FROM t_partner)
     AND prog.status <> 'not_accepted'
$$;
