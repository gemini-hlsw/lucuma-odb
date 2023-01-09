
-- A join table for observations and science (asterism) targets.  Since you
-- cannot simply join any observation with any target (they must reference the
-- same program), it includes a shared program id column.
create table t_asterism_target (

  c_program_id     d_program_id     not null,
  c_observation_id d_observation_id not null,
  c_target_id      d_target_id      not null,

  foreign key (c_program_id, c_observation_id) references t_observation(c_program_id, c_observation_id),
  foreign key (c_program_id, c_target_id)      references t_target(c_program_id, c_target_id),
  constraint t_asterism_target_pkey primary key (c_program_id, c_observation_id, c_target_id)

);

-- Whenever we insert, update or delete a row in t_asterism_target, update the t_asterism column
-- in t_observation so we can use it for the asterism group business below.
CREATE OR REPLACE FUNCTION asterism_update()
  RETURNS trigger AS $$
DECLARE
  obsid d_observation_id;
BEGIN
  IF TG_OP = 'DELETE' THEN
    obsid := OLD.c_observation_id;
  ELSE
    obsid := NEW.c_observation_id;
  END IF;
  update t_observation a 
  set c_asterism_group = coalesce(
    -- ensure that the lists are sorted so we can compare them
    (select to_json(array_agg(b.c_target_id order by b.c_target_id))::jsonb
    from t_asterism_target b
    where a.c_observation_id = b.c_observation_id),
    '[]'::jsonb
  ), c_title = (
    select array_to_string(
      coalesce(
          array_agg(coalesce(t.c_name, 'Unnamed') order by t.c_target_id), 
          array['Untargeted']
      ), 
      ', '
    )
    from t_asterism_target b
    join t_target t on b.c_target_id = t.c_target_id
    where a.c_observation_id = b.c_observation_id
  )
  where a.c_observation_id = obsid;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER asterism_update
  AFTER INSERT OR UPDATE OR DELETE ON t_asterism_target
  DEFERRABLE
  FOR EACH ROW
  EXECUTE PROCEDURE asterism_update();

-- A view keyed on program id and asterism group, with a link to one observation in the group,
-- through which we can get to the targets.
create view v_asterism_group as
  select c_program_id, c_asterism_group, min(c_observation_id)::d_observation_id c_example_observation_id 
  from t_observation 
  group by c_program_id, c_asterism_group;

