
drop view v_program;

alter table t_program
add column c_dismissed_warnings d_tag[] not null default '{}';

create view v_program as
  select
    p.*,
    coalesce(rc.c_resource_count, 0) as c_resource_count,
    ((now() at time zone 'UTC')::date between p.c_active_start and p.c_active_end) as c_is_active
  from t_program p
  left join t_program_resource_count rc on rc.c_program_id = p.c_program_id;
