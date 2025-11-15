-- Fix ch_observation_edit and ch_target_edit should return to OLD for DELETE operations

CREATE OR REPLACE FUNCTION ch_observation_edit()
  RETURNS trigger AS $$
DECLARE
BEGIN
  IF (TG_OP = 'DELETE') THEN
    PERFORM pg_notify('ch_observation_edit', OLD.c_observation_id || ',' || OLD.c_program_id  || ',' || TG_OP);
  ELSEIF ((TG_OP = 'INSERT' OR TG_OP = 'UPDATE') AND (ROW(NEW.*) IS DISTINCT FROM ROW(OLD.*))) THEN
    PERFORM pg_notify('ch_observation_edit', NEW.c_observation_id || ',' || NEW.c_program_id  || ',' || TG_OP);
  END IF;
  -- In case of delete return old
  RETURN COALESCE(NEW, OLD);
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION ch_target_edit()
  RETURNS trigger AS $$
DECLARE
BEGIN
  IF (TG_OP = 'DELETE') THEN
    PERFORM pg_notify('ch_target_edit', OLD.c_target_id || ',' || OLD.c_program_id  || ',' || TG_OP);
  ELSEIF ((TG_OP = 'INSERT' OR TG_OP = 'UPDATE') AND (ROW(NEW.*) IS DISTINCT FROM ROW(OLD.*))) THEN
    PERFORM pg_notify('ch_target_edit', NEW.c_target_id || ',' || NEW.c_program_id  || ',' || TG_OP);
  END IF;
  -- In case of delete return old
  RETURN COALESCE(NEW, OLD);
END;
$$ LANGUAGE plpgsql;

-- Fix ch_observation_edit_associated_table_no_pid_update and ch_observation_edit_timing_windows
-- to skip notification on CASCADE DELETE when the parent observation has already been deleted. 
-- Otherwise we'd send a notificiaation with no contentt
CREATE OR REPLACE FUNCTION ch_observation_edit_associated_table_no_pid_update()
  RETURNS trigger AS $$
DECLARE
  observation record;
  program_id d_program_id;
BEGIN
  observation := COALESCE(NEW, OLD);
  IF ROW(NEW.*) IS DISTINCT FROM ROW(OLD.*) THEN
    -- For DELETE operations during CASCADE, the parent observation may already be gone
    IF TG_OP = 'DELETE' THEN
      program_id := (SELECT c_program_id FROM t_observation WHERE c_observation_id = observation.c_observation_id);
      -- check if the pid still exist
      IF program_id IS NOT NULL THEN
        PERFORM pg_notify('ch_observation_edit', observation.c_observation_id || ',' || program_id  || ',' || 'UPDATE');
      END IF;
    ELSE
      program_id := (SELECT c_program_id FROM t_observation WHERE c_observation_id = observation.c_observation_id);
      PERFORM pg_notify('ch_observation_edit', observation.c_observation_id || ',' || program_id  || ',' || 'UPDATE');
    END IF;
  END IF;
  RETURN observation;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION ch_observation_edit_timing_windows()
  RETURNS trigger AS $$
DECLARE
  observation record;
  program_id d_program_id;
BEGIN
  observation := COALESCE(NEW, OLD);
  IF ROW(NEW.*) IS DISTINCT FROM ROW(OLD.*) THEN
    program_id := (SELECT c_program_id FROM t_observation WHERE c_observation_id = observation.c_observation_id);
    -- check if the pid still exist
    IF program_id IS NOT NULL THEN
      PERFORM pg_notify('ch_observation_edit', observation.c_observation_id || ',' || program_id  || ',' || 'UPDATE');
    END IF;
  END IF;
  RETURN observation;
END;
$$ LANGUAGE plpgsql;
