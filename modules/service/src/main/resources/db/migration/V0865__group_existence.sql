
alter table t_group
add column c_existence e_existence  not null default 'present';

-- need to re-create this view to pick up the new column
drop view v_group;
create view v_group as
  select *,
  case when c_min_interval is not null then c_group_id end as c_min_interval_id,
  case when c_max_interval is not null then c_group_id end as c_max_interval_id
  from t_group;

-- also need to pick up the existence columns here
create or replace view v_group_element as
  select 
    c_program_id || ':' || coalesce(c_parent_id, 'null') || ':' || c_parent_index as c_group_element_id,
    c_program_id,
    c_parent_id as c_group_id, 
    c_parent_index as c_index, 
    c_group_id as c_child_group_id,
    null::d_observation_id as c_child_observation_id,
    c_existence
  from t_group
  union
  select 
    c_program_id || ':' || coalesce(c_group_id, 'null') || ':' || c_group_index,
    c_program_id, 
    c_group_id, 
    c_group_index, 
    null, 
    c_observation_id,
    c_existence
  from t_observation;
