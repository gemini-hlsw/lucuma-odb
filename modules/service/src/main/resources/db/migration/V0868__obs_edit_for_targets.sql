
-- updates this function from V-180 to also trigger a ch_observation_edit for any observation
-- which has the edited target in it's asterism

CREATE OR REPLACE FUNCTION target_update()
  RETURNS trigger AS $$
DECLARE
  tid d_target_id;
  oid d_observation_id;
BEGIN
  tid := NEW.c_target_id;

  -- update the titles of observations that use this target
  if (OLD.c_name != NEW.c_name OR OLD.c_existence != NEW.c_existence) THEN
    -- If one of the above changes, it indirectly triggers ch_observation_edit by modifying the obs title
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
  ELSE
    -- We'll directly trigger ch_observation_edit
    FOR oid in 
      select c_observation_id
      from t_asterism_target
      where c_target_id = tid
    LOOP
      PERFORM pg_notify('ch_observation_edit', oid || ',' || NEW.c_program_id  || ',' || 'UPDATE');
    END LOOP;
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

