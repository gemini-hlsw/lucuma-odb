-- An NGO user may read any program in which their partner requests time
-- (has a non-zero partner split; Explore stores a 0% row for every partner)
-- or holds a non-zero allocation.  Partner
-- splits sit behind the GraphQL ProposalType union, which a Grackle predicate
-- cannot traverse, so the partners are summarized here as a delimited list
-- (e.g. ',ca,us,') that ProgramPredicates.isVisibleTo matches with LIKE.
-- Partner tags and their time accounting category tags coincide.
--
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
      (select ',' || coalesce(string_agg(x.c_partner, ',' order by x.c_partner), '') || ','
         from (
           select s.c_partner
             from t_partner_split s
            where s.c_program_id = p.c_program_id
              and s.c_percent > 0
           union
           select a.c_ta_category
             from t_allocation a
            where a.c_program_id = p.c_program_id
              and a.c_duration > 'PT'
         ) x) as c_ngo_partners
    from t_program p
    left join t_program_resource_count rc on rc.c_program_id = p.c_program_id
  ) q;
