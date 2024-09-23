
-- Updates function from V0080 to exclude "deleted" targets from observation title.

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
      and t.c_existence = 'present'
  )
  where a.c_observation_id = obsid;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;
