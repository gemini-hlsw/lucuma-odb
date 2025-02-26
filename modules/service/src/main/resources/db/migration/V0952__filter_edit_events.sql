-- Update event notify to first compare values

CREATE OR REPLACE FUNCTION ch_observation_edit_associated_table_update()
  RETURNS trigger AS $$
DECLARE
  rec record;
BEGIN
  rec := COALESCE(NEW, OLD);
  IF ROW(NEW.*) IS DISTINCT FROM ROW(OLD.*) THEN
    PERFORM pg_notify('ch_observation_edit', rec.c_observation_id || ',' || rec.c_program_id  || ',' || 'UPDATE');
  END IF;
  RETURN rec;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION ch_observation_edit()
  RETURNS trigger AS $$
DECLARE
BEGIN
  IF (TG_OP = 'DELETE') THEN
    PERFORM pg_notify('ch_observation_edit', OLD.c_observation_id || ',' || OLD.c_program_id  || ',' || TG_OP);
  ELSEIF ((TG_OP = 'INSERT' OR TG_OP = 'UPDATE') AND (ROW(NEW.*) IS DISTINCT FROM ROW(OLD.*))) THEN
    PERFORM pg_notify('ch_observation_edit', NEW.c_observation_id || ',' || NEW.c_program_id  || ',' || TG_OP);
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION ch_observation_edit_timing_windows()
  RETURNS trigger AS $$
DECLARE
  observation record;
  program_id d_program_id;
BEGIN
  observation := COALESCE(NEW, OLD);
  program_id := (SELECT c_program_id FROM t_observation WHERE c_observation_id = observation.c_observation_id);
  IF ROW(NEW.*) IS DISTINCT FROM ROW(OLD.*) THEN
    PERFORM pg_notify('ch_observation_edit', observation.c_observation_id || ',' || program_id  || ',' || 'UPDATE');
  END IF;
  RETURN observation;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION ch_allocation_edit()
  RETURNS trigger AS $$
DECLARE
  allocation record;
BEGIN
  allocation := COALESCE(NEW, OLD);
  IF ROW(NEW.*) IS DISTINCT FROM ROW(OLD.*) THEN
    PERFORM pg_notify('ch_program_edit', allocation.c_program_id || ',' || 'UPDATE');
  END IF;
  RETURN allocation;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION ch_email_edit()
  RETURNS trigger AS $$
DECLARE
  email record;
BEGIN
  email := COALESCE(NEW, OLD);
  -- Since this is a programEdit subscription, it is always an UPDATE to the program
  IF ROW(NEW.*) IS DISTINCT FROM ROW(OLD.*) THEN
    PERFORM pg_notify('ch_program_edit', email.c_program_id || ',' || 'UPDATE');
  END IF;
  RETURN email;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION ch_invitation_edit()
  RETURNS trigger AS $$
DECLARE
  invitation record;
BEGIN
  invitation := COALESCE(NEW, OLD);
  -- Since this is a programEdit subscription, it is always an UPDATE to the program
  IF ROW(NEW.*) IS DISTINCT FROM ROW(OLD.*) THEN
    PERFORM pg_notify('ch_program_edit', invitation.c_program_id || ',' || 'UPDATE');
  END IF;
  RETURN invitation;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION ch_program_user_edit()
  RETURNS trigger AS $$
DECLARE
  proguser record;
BEGIN
  proguser := COALESCE(NEW, OLD);
  -- Since this is a programEdit subscription, it is always an UPDATE to the program
  IF ROW(NEW.*) IS DISTINCT FROM ROW(OLD.*) THEN
    PERFORM pg_notify('ch_program_edit', proguser.c_program_id || ',' || 'UPDATE');
  END IF;
  RETURN proguser;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION ch_partner_splits_edit()
  RETURNS trigger AS $$
DECLARE
  splits record;
BEGIN
  splits := COALESCE(NEW, OLD);
  -- Since this is a programEdit subscription, it is always an UPDATE to the program
  IF ROW(NEW.*) IS DISTINCT FROM ROW(OLD.*) THEN
    PERFORM pg_notify('ch_program_edit', splits.c_program_id || ',' || 'UPDATE');
  END IF;
  RETURN splits;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION ch_proposal_edit()
  RETURNS trigger AS $$
DECLARE
  proposal record;
BEGIN
  proposal := COALESCE(NEW, OLD);
  -- Since this is a programEdit subscription, it is always an UPDATE to the program
  IF ROW(NEW.*) IS DISTINCT FROM ROW(OLD.*) THEN
    PERFORM pg_notify('ch_program_edit', proposal.c_program_id || ',' || 'UPDATE');
  END IF;
  RETURN proposal;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION ch_program_edit()
  RETURNS trigger AS $$
DECLARE
BEGIN
  IF ROW(NEW.*) IS DISTINCT FROM ROW(OLD.*) THEN
    PERFORM pg_notify('ch_program_edit',  NEW.c_program_id || ',' || TG_OP);
  END IF;
  RETURN NEW; -- n.b. doesn't matter, it's an AFTER trigger
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
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;