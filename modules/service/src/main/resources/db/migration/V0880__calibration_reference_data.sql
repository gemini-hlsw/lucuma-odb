-- Add an enum to indicate if a target has a calibration role
create type e_calibration_role as enum('twilight', 'photometric', 'spectrophotometric', 'telluric');
comment on type e_calibration_role is 'The role of the target in a calibration.';

alter table t_target
drop column c_role cascade;

alter table t_target
add column c_calibration_role e_calibration_role null;

-- Drop constraints/trigger that are no longer needed because c_role is dropped
alter table t_asterism_target 
drop constraint asterism_target_is_science;

drop trigger t_target_role_immutable_trigger on t_target;

-- Need to recreate target view because of added and dropped field in target table.
drop view if exists v_target;

-- a view that has synthetic nullable ids for nullable embedded objects (required by grackle)
-- Same as in V0070
create view v_target as
  select *,
  case when c_type='sidereal'              then c_target_id end as c_sidereal_id,
  case when c_type='nonsidereal'           then c_target_id end as c_nonsidereal_id,
  case when c_sid_catalog_name is not null then c_target_id end as c_sid_catalog_info_id,
  case when c_sid_pm_ra        is not null then c_target_id end as c_sid_pm_id,
  case when c_sid_parallax     is not null then c_target_id end as c_sid_parallax_id,
  case when c_sid_rv           is not null then c_target_id end as c_sid_rv_id
  from t_target;
