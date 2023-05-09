CREATE OR REPLACE FUNCTION ch_group_edit()
  RETURNS trigger AS $$
DECLARE
BEGIN
  PERFORM pg_notify('ch_group_edit', NEW.c_group_id || ',' || NEW.c_program_id || ',' || nextval('s_event_id')::text || ',' || TG_OP);
  RETURN NEW; -- n.b. doesn't matter, it's an AFTER trigger
END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER ch_group_edit_trigger
  AFTER INSERT OR UPDATE ON t_group
  DEFERRABLE
  FOR EACH ROW
  EXECUTE PROCEDURE ch_group_edit();
