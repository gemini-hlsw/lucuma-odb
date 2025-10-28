
-- Update to trigger on hatrd delete

CREATE OR REPLACE FUNCTION ch_group_edit()
  RETURNS trigger AS $$
DECLARE
BEGIN

  IF (TG_OP = 'DELETE') THEN
    PERFORM pg_notify('ch_group_edit', OLD.c_group_id || ',' || OLD.c_program_id || ',' || TG_OP);
  ELSE
    PERFORM pg_notify('ch_group_edit', NEW.c_group_id || ',' || NEW.c_program_id || ',' || TG_OP);
  END IF;

  -- sc-5589:If the parent group has changed send an update event for the old parent.
  IF (NEW.c_parent_id IS DISTINCT FROM OLD.c_parent_id and OLD.c_parent_id IS NOT NULL) THEN
    PERFORM pg_notify('ch_group_edit', OLD.c_parent_id || ',' || NEW.c_program_id || ',' || 'UPDATE');
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER ch_group_edit_trigger ON t_group;

CREATE CONSTRAINT TRIGGER ch_group_edit_trigger
  AFTER INSERT OR UPDATE OR DELETE ON t_group
  DEFERRABLE
  FOR EACH ROW
  EXECUTE PROCEDURE ch_group_edit();
