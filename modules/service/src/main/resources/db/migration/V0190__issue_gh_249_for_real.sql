
-- sorry, V0180__issue_gh_249.sql should have been 250 but we can't rename it
-- https://github.com/gemini-hlsw/lucuma-odb/issues/249

CREATE OR REPLACE FUNCTION asterism_update_for_target_existence()
  RETURNS trigger AS $$
DECLARE
  tid d_target_id;
BEGIN
  tid := NEW.c_target_id;

  if (OLD.c_existence != NEW.c_existence) THEN
    update t_observation a 
    set c_asterism_group = coalesce(
      (
        select to_json(array_agg(b.c_target_id order by b.c_target_id))::jsonb
        from t_asterism_target b
        join t_target t on b.c_target_id = t.c_target_id
        where a.c_observation_id = b.c_observation_id
        and t.c_existence = 'present'
      ),
      '[]'::jsonb
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

CREATE CONSTRAINT TRIGGER ch_target_edit_trigger_for_asterism
  AFTER UPDATE ON t_target
  DEFERRABLE
  FOR EACH ROW
  EXECUTE PROCEDURE asterism_update_for_target_existence();

