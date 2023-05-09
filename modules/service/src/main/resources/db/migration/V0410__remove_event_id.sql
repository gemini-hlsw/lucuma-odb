
-- Update these triggers to remove the id, which we don't need

CREATE OR REPLACE FUNCTION ch_program_edit()
  RETURNS trigger AS $$
DECLARE
BEGIN
  PERFORM pg_notify('ch_program_edit',  NEW.c_program_id || ',' || TG_OP);
  RETURN NEW; -- n.b. doesn't matter, it's an AFTER trigger
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION ch_target_edit()
  RETURNS trigger AS $$
DECLARE
BEGIN
  PERFORM pg_notify('ch_target_edit', NEW.c_target_id || ',' || NEW.c_program_id  || ',' || TG_OP);
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION ch_observation_edit()
  RETURNS trigger AS $$
DECLARE
BEGIN
  PERFORM pg_notify('ch_observation_edit', NEW.c_observation_id || ',' || NEW.c_program_id  || ',' || TG_OP);
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;
