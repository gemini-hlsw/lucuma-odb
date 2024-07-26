DROP TRIGGER ch_observation_edit_trigger on t_observation;
DROP FUNCTION ch_observation_edit();

CREATE OR REPLACE FUNCTION ch_observation_edit()
  RETURNS trigger AS $$
DECLARE
BEGIN
  IF (TG_OP = 'DELETE') THEN
    PERFORM pg_notify('ch_observation_edit', OLD.c_observation_id || ',' || OLD.c_program_id  || ',' || TG_OP);
  END IF;
  IF (TG_OP = 'INSERT' OR TG_OP = 'UPDATE') THEN
    PERFORM pg_notify('ch_observation_edit', NEW.c_observation_id || ',' || NEW.c_program_id  || ',' || TG_OP);
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER ch_observation_edit_trigger
  AFTER INSERT OR UPDATE OR DELETE ON t_observation
  DEFERRABLE
  FOR EACH ROW
  EXECUTE PROCEDURE ch_observation_edit();

CREATE OR REPLACE FUNCTION ch_group_edit()
  RETURNS trigger AS $$
DECLARE
  thegroup record;
BEGIN
  thegroup := COALESCE(NEW, OLD);
  PERFORM pg_notify('ch_group_edit', thegroup.c_group_id || ',' || thegroup.c_program_id || ',' || TG_OP);
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER ch_group_edit_trigger on t_group;
CREATE CONSTRAINT TRIGGER ch_group_edit_trigger
  AFTER INSERT OR UPDATE ON t_group
  DEFERRABLE
  FOR EACH ROW
  EXECUTE PROCEDURE ch_group_edit();

DROP TRIGGER ch_obs_group_edit_trigger ON t_observation;
DROP FUNCTION ch_obs_group_edit();
CREATE OR REPLACE FUNCTION ch_obs_group_edit()
  RETURNS trigger AS $$
DECLARE
BEGIN
  -- notify old group, if any
  IF (TG_OP = 'UPDATE' OR TG_OP = 'DELETE') THEN
    PERFORM pg_notify('ch_group_edit', coalesce(OLD.c_group_id, 'null') || ',' || coalesce(NEW.c_program_id, OLD.c_program_id, 'null') || ',' || TG_OP);
  END IF;
  -- notify new group, if any
  IF (TG_OP = 'INSERT' OR TG_OP = 'UPDATE' OR TG_OP = 'DELETE') THEN
      -- PERFORM pg_notify('ch_group_edit', TG_OP || coalesce(NEW.c_group_id, 'null1') || ',' || coalesce(OLD.c_group_id, 'null2') || ',' || coalesce(NEW.c_program_id, 'null3') || ',' || coalesce(OLD.c_program_id, 'null3') || ',' ||  TG_OP);
      PERFORM pg_notify('ch_group_edit', coalesce(NEW.c_group_id, 'null') || ',' || coalesce(NEW.c_program_id, OLD.c_program_id, 'null') || ',' || TG_OP);
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
