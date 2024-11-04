
CREATE OR REPLACE FUNCTION ch_target_edit()
  RETURNS trigger AS $$
DECLARE
BEGIN
  IF (TG_OP = 'DELETE') THEN
    PERFORM pg_notify('ch_target_edit', OLD.c_target_id || ',' || OLD.c_program_id  || ',' || TG_OP);
  END IF;
  IF (TG_OP = 'INSERT' OR TG_OP = 'UPDATE') THEN
    PERFORM pg_notify('ch_target_edit', NEW.c_target_id || ',' || NEW.c_program_id  || ',' || TG_OP);
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS ch_target_insert_update_trigger on t_target;

CREATE CONSTRAINT TRIGGER ch_target_insert_update_trigger
  AFTER INSERT OR UPDATE OR DELETE ON t_target
  DEFERRABLE
  FOR EACH ROW
  EXECUTE PROCEDURE ch_target_edit();



