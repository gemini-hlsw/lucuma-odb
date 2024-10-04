CREATE OR REPLACE FUNCTION ch_allocation_edit()
  RETURNS trigger AS $$
DECLARE
  allocation record;
BEGIN
  allocation := COALESCE(NEW, OLD);
  PERFORM pg_notify('ch_program_edit', allocation.c_program_id || ',' || 'UPDATE');
  RETURN allocation;
END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER ch_program_edit_allocation_trigger
  AFTER INSERT OR UPDATE OR DELETE ON t_allocation
  DEFERRABLE
  FOR EACH ROW
  EXECUTE PROCEDURE ch_allocation_edit();