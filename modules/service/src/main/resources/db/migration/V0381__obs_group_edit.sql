-- trigger for group edit event, when an observation is created or is moved
CREATE OR REPLACE FUNCTION ch_obs_group_edit()
  RETURNS trigger AS $$
DECLARE
BEGIN
  -- notify old group, if any
  IF (TG_OP = 'UPDATE' OR TG_OP = 'DELETE') THEN
    PERFORM pg_notify('ch_group_edit', coalesce(OLD.c_group_id, 'null') || ',' || NEW.c_program_id || ',' || 'UPDATE');
  END IF;
  -- notify new group, if any
  IF (TG_OP = 'INSERT' OR TG_OP = 'UPDATE' OR TG_OP = 'DELETE') THEN
      PERFORM pg_notify('ch_group_edit', coalesce(NEW.c_group_id, 'null') || ',' || NEW.c_program_id || ',' || 'UPDATE');
  END IF;
  RETURN NEW; -- n.b. doesn't matter, it's an AFTER trigger
END;
$$ LANGUAGE plpgsql;

-- call the trigger when an observation is added or deleted, or if the group columns are updated
CREATE CONSTRAINT TRIGGER ch_obs_group_edit_trigger
  AFTER INSERT
     OR UPDATE OF c_group_id, c_group_index
     OR DELETE 
     ON t_observation
  DEFERRABLE
  FOR EACH ROW
  EXECUTE PROCEDURE ch_obs_group_edit();
