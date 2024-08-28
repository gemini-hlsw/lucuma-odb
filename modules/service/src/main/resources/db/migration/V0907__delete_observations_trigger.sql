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

