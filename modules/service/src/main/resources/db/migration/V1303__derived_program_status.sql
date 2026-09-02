-- Program status becomes a derived value: ACTIVE/INACTIVE computed from the
-- active period, with an optional explicit staff override.  The stored column
-- holds only the override; the effective status lives on v_program.

alter table t_program
rename column c_program_status to c_explicit_status;

alter table t_program
alter column c_explicit_status drop default,
alter column c_explicit_status drop not null;

-- Existing rows only ever held the old ACTIVE default, which is now expressed
-- by the absence of an override.
update t_program
set c_explicit_status = null
where c_explicit_status = 'active';

-- The view gains computed status columns and drops c_is_active (V1250), which
-- c_status = 'active' supersedes, so it must be dropped and recreated.
drop view v_program;

create view v_program as
  select
    q.*,
    coalesce(q.c_explicit_status, q.c_default_status) as c_status
  from (
    select
      p.*,
      coalesce(rc.c_resource_count, 0) as c_resource_count,
      (case
         when (now() at time zone 'UTC')::date between p.c_active_start and p.c_active_end then 'active'
         else 'inactive'
       end)::d_tag as c_default_status
    from t_program p
    left join t_program_resource_count rc on rc.c_program_id = p.c_program_id
  ) q;
