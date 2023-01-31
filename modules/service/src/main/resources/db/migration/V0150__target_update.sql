
CREATE OR REPLACE FUNCTION ch_target_edit()
  RETURNS trigger AS $$
DECLARE
BEGIN
  PERFORM pg_notify('ch_target_edit', NEW.c_target_id || ',' || NEW.c_program_id  || ',' || nextval('s_event_id')::text || ',' || TG_OP);
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER ch_target_insert_update_trigger
  AFTER INSERT OR UPDATE ON t_target
  DEFERRABLE
  FOR EACH ROW
  EXECUTE PROCEDURE ch_target_edit();



