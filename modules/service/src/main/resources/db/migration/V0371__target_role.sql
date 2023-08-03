create type e_target_role as enum('science', 'guide');
comment on type e_existence is 'The role of the target.';

alter table t_target
add column c_role e_target_role;

update t_target
set c_role = 'science';

alter table t_target
alter column c_role set not null;

CREATE OR REPLACE FUNCTION asterism_target_is_science(d_target_id)
  RETURNS BOOLEAN AS $$
DECLARE
  role e_target_role;
BEGIN
  SELECT c_role
  INTO role
  FROM t_target
  WHERE c_target_id = $1;
  IF FOUND AND (role != 'science') THEN
    RAISE EXCEPTION 'Asterism target must be a science target.';
  ELSE
    RETURN TRUE;
  END IF;
END;
$$ LANGUAGE plpgsql;

alter table t_asterism_target 
add constraint asterism_target_is_science check (asterism_target_is_science(c_target_id));

-- The above prevents assigning a non-science target to an asterism, but doesn't protect
-- against the role of a target changing.
CREATE OR REPLACE FUNCTION target_role_immutable()
  RETURNS trigger AS $$
DECLARE
BEGIN
  IF NEW.c_role != OLD.c_role THEN
    RAISE EXCEPTION 'The role of a target cannot be changed.';
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER t_target_role_immutable_trigger
BEFORE UPDATE on t_target
FOR EACH ROW
EXECUTE FUNCTION target_role_immutable();

-- Need to recreate target view because of added field in target table.
-- Can't use `update_table_with_views` because the saved view definition has
-- expanded the `*` to the explicit columns, so the new column won't get added.
drop view v_target;

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
