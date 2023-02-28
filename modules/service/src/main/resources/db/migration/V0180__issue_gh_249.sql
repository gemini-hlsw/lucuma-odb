
-- https://github.com/gemini-hlsw/lucuma-odb/issues/250

-- updates this function to recompute the name on existence changes, and reflect these changes
-- in the new updated name

CREATE OR REPLACE FUNCTION target_update()
  RETURNS trigger AS $$
DECLARE
  tid d_target_id;
BEGIN
  tid := NEW.c_target_id;

  -- update the titles of observations that use this target
  if (OLD.c_name != NEW.c_name OR OLD.c_existence != NEW.c_existence) THEN
    update t_observation a 
    set c_title = (
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
    where a.c_observation_id in (
      select c_observation_id
      from t_asterism_target
      where c_target_id = tid
    );
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

