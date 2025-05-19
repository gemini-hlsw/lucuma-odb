-- Previous migration: V0330
CREATE OR REPLACE FUNCTION ch_group_edit()
  RETURNS trigger AS $$
DECLARE
BEGIN
  PERFORM pg_notify('ch_group_edit', NEW.c_group_id || ',' || NEW.c_program_id || ',' || TG_OP);

  -- sc-5589:If the parent group has changed send an update event for the old parent.
  IF (NEW.c_parent_id IS DISTINCT FROM OLD.c_parent_id and OLD.c_parent_id IS NOT NULL) THEN
    PERFORM pg_notify('ch_group_edit', OLD.c_parent_id || ',' || NEW.c_program_id || ',' || 'UPDATE');
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Previous migration: V0381
CREATE OR REPLACE FUNCTION ch_obs_group_edit()
  RETURNS trigger AS $$
DECLARE
BEGIN
  -- sc-5589: Events with a null group id do not need to be sent.
  -- notify old group, if any
  IF ((TG_OP = 'UPDATE' OR TG_OP = 'DELETE') AND (OLD.c_group_id IS NOT NULL)) THEN
    PERFORM pg_notify('ch_group_edit', OLD.c_group_id || ',' || NEW.c_program_id || ',' || 'UPDATE');
  END IF;

  -- notify new group, if any
  IF ((TG_OP = 'INSERT' OR TG_OP = 'UPDATE' OR TG_OP = 'DELETE') AND (NEW.c_group_id IS NOT NULL)) THEN
      PERFORM pg_notify('ch_group_edit', NEW.c_group_id || ',' || NEW.c_program_id || ',' || 'UPDATE');
  END IF;
  RETURN NEW; -- n.b. doesn't matter, it's an AFTER trigger
END;
$$ LANGUAGE plpgsql;
 