
alter table t_group
add column c_system boolean  not null default false;

-- need to re-create this view to pick up the new column
drop view v_group;
create view v_group as
  select *,
  case when c_min_interval is not null then c_group_id end as c_min_interval_id,
  case when c_max_interval is not null then c_group_id end as c_max_interval_id
  from t_group;

-- Partial unique index to enforce that system groups are unique within a program
create unique index unique_system_group_name on t_group (c_program_id, c_name) where c_system is true;

