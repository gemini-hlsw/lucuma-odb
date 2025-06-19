

-- Add new enum case
alter type e_target_type add value 'opportunity';

-- Add enum tag type for arc types
create type e_arc_type as enum('empty', 'full', 'partial');

-- Add columns for region, which cannot be null for TOOs
alter table t_target
  -- RA arc
  add c_opp_ra_arc_type  e_arc_type  null check (c_type = 'opportunity'        or c_opp_ra_arc_type  is null),
  add c_opp_ra_arc_start d_angle_µas null check (c_opp_ra_arc_type = 'partial' or c_opp_ra_arc_start is null),
  add c_opp_ra_arc_end   d_angle_µas null check (c_opp_ra_arc_type = 'partial' or c_opp_ra_arc_end   is null),
  -- Dec arc
  add c_opp_dec_arc_type  e_arc_type  null check (c_type = 'opportunity'         or c_opp_dec_arc_type  is null),
  add c_opp_dec_arc_start d_angle_µas null check (c_opp_dec_arc_type = 'partial' or c_opp_dec_arc_start is null),
  add c_opp_dec_arc_end   d_angle_µas null check (c_opp_dec_arc_type = 'partial' or c_opp_dec_arc_end   is null),

  drop constraint nonsidereal_all_non_null, -- TODO
  drop constraint ra_dec_epoch_all_defined; -- TODO 

-- Update target view 
drop view if exists v_target;
create view v_target as
  select *,
  case when c_sid_catalog_name is not null then c_target_id end as c_sid_catalog_info_id,
  case when c_sid_pm_ra        is not null then c_target_id end as c_sid_pm_id,
  case when c_sid_parallax     is not null then c_target_id end as c_sid_parallax_id,
  case when c_sid_rv           is not null then c_target_id end as c_sid_rv_id,
  case when c_type='sidereal'              then c_target_id end as c_sidereal_id,
  case when c_type='nonsidereal'           then c_target_id end as c_nonsidereal_id,
  case when c_type='opportunity'           then c_target_id end as c_opportunity_id,
  case when c_type='opportunity' then c_target_id end as c_opportunity_dec_arc_synthetic_id,
  case when c_type='opportunity' then c_target_id end as c_opportunity_ra_arc_synthetic_id,
  case when c_type='opportunity' and c_opp_dec_arc_type = 'partial' then c_target_id end as c_opportunity_dec_arc_start_end_synthetic_id,
  case when c_type='opportunity' and c_opp_ra_arc_type  = 'partial' then c_target_id end as c_opportunity_ra_arc_start_end_synthetic_id
  from t_target;

