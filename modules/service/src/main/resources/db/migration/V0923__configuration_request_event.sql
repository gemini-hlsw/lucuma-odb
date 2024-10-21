

CREATE OR REPLACE FUNCTION ch_configuration_request_edit()
  RETURNS trigger AS $$
DECLARE
  rec record;
BEGIN
  rec := COALESCE(NEW, OLD);
  PERFORM pg_notify('ch_configuration_request_edit',  rec.c_configuration_request_id || ',' || rec.c_program_id || ',' || TG_OP);
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER ch_configuration_request_edit_trigger
  AFTER INSERT OR UPDATE OR DELETE ON t_configuration_request
  DEFERRABLE
  FOR EACH ROW
  EXECUTE PROCEDURE ch_configuration_request_edit();

  