-- Add to t_target
alter table t_target
add column c_target_disposition e_target_disposition;

update t_target set c_target_disposition =
  case
    when c_calibration_role is not null then 'calibration'::e_target_disposition
    else 'science'::e_target_disposition
  end;

alter table t_target
alter column c_target_disposition set not null;

-- Set default value
alter table t_target
alter column c_target_disposition set default 'science'::e_target_disposition;

alter table t_target
add constraint target_disposition_calibration_role_check check (
  (c_target_disposition = 'calibration' AND c_calibration_role IS NOT NULL)
  OR
  (c_target_disposition <> 'calibration' AND c_calibration_role IS NULL)
);

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

-- Update trigger to also set target_disposition when calibration_role is set
CREATE OR REPLACE FUNCTION calibration_targets_on_calibration_programs()
  RETURNS trigger AS $$
DECLARE
    calibration_role e_calibration_role;
BEGIN
    -- Fetch the value from the source table
    SELECT c_calibration_role INTO calibration_role
    FROM t_program
    WHERE c_program_id = NEW.c_program_id;

    -- Only set NEW.c_calibration_role if calibration_role is not null
    IF calibration_role IS NOT NULL THEN
        NEW.c_calibration_role := calibration_role;
        NEW.c_target_disposition := 'calibration';
    END IF;

    RETURN NEW;
END;
$$ LANGUAGE plpgsql;
