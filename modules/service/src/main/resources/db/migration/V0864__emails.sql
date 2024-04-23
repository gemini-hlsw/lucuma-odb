-- Status of an email
CREATE TYPE e_email_status AS ENUM (
  'queued',
  'rejected',
  'accepted',
  'delivered',
  'permanent_failure',
  'temporary_failure'
);
COMMENT ON TYPE e_email_status IS 'Status of emails.';

CREATE TABLE t_email (
  c_email_id          TEXT           NOT NULL PRIMARY KEY CHECK(length(c_email_id) > 0),
  c_program_id        d_program_id   NOT NULL REFERENCES t_program (c_program_id),
  c_sender_email      d_email        NOT NULL,
  c_recipient_email   d_email        NOT NULL,
  c_subject           TEXT           NOT NULL             CHECK(length(c_subject) > 0),
  c_text_message      TEXT           NOT NULL             CHECK(length(c_text_message) > 0),
  c_html_message      TEXT           NULL                 CHECK(c_html_message IS NULL OR length(c_html_message) > 0),
  c_original_time     timestamp      NOT NULL DEFAULT now(),
  c_status            e_email_status NOT NULL,
  c_status_time       timestamp      NOT NULL DEFAULT now()
);

-- We'll usually set this from the email webhook, but if we do it manually we'll set it to now.
CREATE OR REPLACE FUNCTION ch_update_email_status_time()
  RETURNS trigger AS $$
DECLARE
BEGIN
  IF NEW.c_status IS DISTINCT FROM OLD.c_status AND NEW.c_status_time IS NULL THEN
   NEW.c_status_time = now();
  END IF;
  return NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER ch_update_email_status_time_trigger
  BEFORE UPDATE on t_email
  FOR EACH ROW
  EXECUTE PROCEDURE ch_update_email_status_time();

-- Add the email id to the invitation

ALTER TABLE t_invitation
  -- Needs to be nullable because we need to add a row to get the hash before we can send the email.
  -- Unfortunately, CHECK constraints cannot be deferred.
  ADD COLUMN c_email_id TEXT NULL REFERENCES t_email (c_email_id);

-- Replace the insert function from V0854 to include the email id
CREATE OR REPLACE FUNCTION insert_invitation(
  p_issuer_id       d_user_id,                   
  p_program_id      d_program_id,         
  p_recipient_email d_email,       
  p_role            e_program_user_role,         
  p_support_type    e_program_user_support_type, 
  p_support_partner d_tag,
  p_email_id        text
) RETURNS text AS $$
  DECLARE
    invitation_key_id d_invitation_id := to_hex(nextval('s_invitation_id'::regclass));
    invitation_key text := md5(random()::text) || md5(random()::text) ||  md5(random()::text);
    invitation_key_hash text := md5(invitation_key);
  BEGIN
    INSERT INTO t_invitation (
      c_invitation_id,
      c_issuer_id,      
      c_program_id,     
      c_recipient_email,
      c_role,           
      c_support_type,   
      c_support_partner,
      c_key_hash,
      c_email_id
    )
    VALUES (
      invitation_key_id,
      p_issuer_id,      
      p_program_id,     
      p_recipient_email,
      p_role,           
      p_support_type,   
      p_support_partner,
      invitation_key_hash,
      p_email_id
    );
    return invitation_key_id || '.' || invitation_key;
  END;
$$ LANGUAGE plpgsql;

-- trigger a program edit notification when t_email changes
CREATE OR REPLACE FUNCTION ch_email_edit()
  RETURNS trigger AS $$
DECLARE
  email record;
BEGIN
  email := COALESCE(NEW, OLD);
  -- Since this is a programEdit subscription, it is always an UPDATE to the program
  PERFORM pg_notify('ch_program_edit', email.c_program_id || ',' || 'UPDATE');
  RETURN email;
END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER ch_program_edit_email_trigger
  AFTER INSERT OR UPDATE OR DELETE ON t_email
  DEFERRABLE
  FOR EACH ROW
  EXECUTE PROCEDURE ch_email_edit();
