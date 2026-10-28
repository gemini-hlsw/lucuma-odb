-- An NGO user may read any program in which their partner requests time or
-- holds time.  program_ngo_partners is the single definition of that rule; it
-- backs v_program.c_ngo_partners (matched with LIKE by
-- ProgramPredicates.isVisibleTo, since partner splits sit behind the GraphQL
-- ProposalType union, which a Grackle predicate cannot traverse), the
-- subscription topics, the cone search and the service-level read checks.
--
-- * Splits count only when non-zero (Explore stores a 0% row for every
--   partner) and only while a proposal exists (deleteProposal leaves the
--   t_partner_split rows behind).
-- * Allocations count when non-zero and when their time accounting category is
--   a partner (partner tags and their category tags coincide; categories such
--   as 'dd' are not partners).
CREATE FUNCTION program_ngo_partners(pid d_program_id) RETURNS SETOF d_tag
LANGUAGE sql STABLE AS $$
  SELECT s.c_partner
    FROM t_partner_split s
   WHERE s.c_program_id = pid
     AND s.c_percent > 0
     AND EXISTS (SELECT 1 FROM t_proposal pr WHERE pr.c_program_id = pid)
  UNION
  SELECT a.c_ta_category
    FROM t_allocation a
   WHERE a.c_program_id = pid
     AND a.c_duration > 'PT'
     AND a.c_ta_category IN (SELECT c_tag FROM t_partner)
$$;

-- Body copied from V1328, plus c_ngo_partners.
DROP VIEW v_program;

CREATE VIEW v_program AS
  select
    q.*,
    coalesce(q.c_explicit_status, q.c_default_status) as c_status
  from (
    select
      p.*,
      coalesce(rc.c_resource_count, 0) as c_resource_count,
      -- Summaries of what the program's observations ask for.  Present,
      -- non-calibration observations only; calibrations never ask for either.
      (select coalesce(max(o.c_too_activation), 'none'::e_too_activation)
         from t_observation o
        where o.c_program_id = p.c_program_id
          and o.c_existence = 'present'
          and o.c_calibration_role is null) as c_max_too_activation,
      (select coalesce(max(o.c_scheduling_mode), 'unconstrained'::e_scheduling_mode)
         from t_observation o
        where o.c_program_id = p.c_program_id
          and o.c_existence = 'present'
          and o.c_calibration_role is null) as c_max_scheduling_mode,
      (case
         when (now() at time zone 'UTC')::date between p.c_active_start and p.c_active_end then 'active'
         else 'inactive'
       end)::d_tag as c_default_status,
      (select ',' || coalesce(string_agg(x, ',' order by x), '') || ','
         from program_ngo_partners(p.c_program_id) x) as c_ngo_partners
    from t_program p
    left join t_program_resource_count rc on rc.c_program_id = p.c_program_id
  ) q;
