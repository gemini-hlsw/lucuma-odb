
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

-- Raise an error if the specified program contains a deleted group that contains non-deleted elements.
CREATE OR REPLACE PROCEDURE group_verify_existence(pid d_program_id) AS $$
DECLARE
  eid text;
BEGIN
  SELECT e.c_group_element_id 
  INTO   eid
  FROM   v_group_element e
  JOIN   t_group g ON e.c_group_id = g.c_group_id 
  WHERE  g.c_existence = 'deleted'
  AND    e.c_existence = 'present'
  AND    g.c_program_id = pid;
  IF FOUND THEN
    RAISE EXCEPTION 'Deleted group contains non-deleted element(s)';
  END IF;
END;
$$ LANGUAGE plpgsql;

-- Add the existence check to our group verification proc.
CREATE OR REPLACE PROCEDURE group_verify(pid d_program_id) AS $$
BEGIN
  CALL group_verify_acyclic(pid);
  CALL group_verify_indices(pid);
  CALL group_verify_existence(pid);
END;
$$ LANGUAGE plpgsql;

-- Replace the group trigger with one that also fires on existence changes.
DROP TRIGGER group_trigger_groups ON t_group;
CREATE CONSTRAINT TRIGGER group_trigger_groups
AFTER INSERT OR DELETE OR UPDATE OF c_program_id, c_parent_id, c_parent_index, c_existence ON t_group
DEFERRABLE
FOR EACH ROW
EXECUTE FUNCTION t_group_trigger();

-- Replace the obs trigger with one that also fires on existence changes.
DROP TRIGGER group_trigger_observations ON t_observation;
CREATE CONSTRAINT TRIGGER group_trigger_observations
AFTER INSERT OR DELETE OR UPDATE OF c_program_id, c_group_id, c_group_index, c_existence ON t_observation
DEFERRABLE
FOR EACH ROW
EXECUTE FUNCTION t_group_trigger();
