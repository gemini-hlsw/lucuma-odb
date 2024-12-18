
-- All invitations will henceforth refer to a t_program_user entry.  So you have
-- to create the program user before an invitation can be sent.  That means the
-- c_user_id will not be known for invited users until and unless they accept
-- the invitation.

-- Existing invitations are now invalid.
TRUNCATE TABLE t_invitation;

-- We'll make the c_user_id nullable.  That means we cannot keep the uniqueness
-- constraint on (c_program_id, c_user_id) for nullable rows.
ALTER TABLE t_program_user
  DROP CONSTRAINT "t_program_user_c_program_id_c_user_id_key";

ALTER TABLE t_program_user
  ALTER COLUMN c_user_id DROP NOT NULL;

-- If there is a user though, it must be unique in the program.
CREATE UNIQUE INDEX unique_program_user ON t_program_user (
  c_program_id,
  c_user_id
) WHERE c_user_id IS NOT NULL;

-- We'll need to refer to the program user from invitations, so we need an id.
CREATE DOMAIN d_program_user_id AS varchar
  CHECK (VALUE ~ '^r-[1-9a-f][0-9a-f]*$');
CREATE SEQUENCE s_program_user_id START WITH 256;

ALTER TABLE t_program_user
  ADD COLUMN c_program_user_id d_program_user_id DEFAULT 'r-' || to_hex(nextval('s_program_user_id'));

-- Update existing rows with the new program/user id.
WITH seq AS (
  SELECT nextval('s_program_user_id') AS seq_val
)
UPDATE t_program_user
   SET c_program_user_id = 'r-' || to_hex(seq.seq_val)
  FROM seq;

-- Make the id the primary key.
ALTER TABLE t_program_user
  ADD PRIMARY KEY (c_program_user_id);

-- Drop the sequence if the table is dropped.
ALTER SEQUENCE s_program_user_id OWNED BY t_program_user.c_program_user_id;

-- Now that every invitation refers to a program user, it doesn't need duplicate
-- program user information.
ALTER TABLE t_invitation
  DROP COLUMN c_role,
  DROP COLUMN c_redeemer_id,
  DROP COLUMN c_partner,
  DROP COLUMN c_partner_link;

-- Add the program user id to the invitation.
ALTER TABLE t_invitation
  ADD COLUMN c_program_user_id d_program_user_id NOT NULL REFERENCES t_program_user(c_program_user_id) ON DELETE CASCADE;

-- Update the invitation creation function.
DROP FUNCTION insert_invitation;

CREATE FUNCTION insert_invitation(
  p_program_user_id d_program_user_id,
  p_issuer_id       d_user_id,
  p_program_id      d_program_id,
  p_recipient_email d_email,
  p_email_id        text
) RETURNS text AS $$
  DECLARE
    invitation_key_id   d_invitation_id := to_hex(nextval('s_invitation_id'::regclass));
    invitation_key      text            := md5(random()::text) || md5(random()::text) ||  md5(random()::text);
    invitation_key_hash text            := md5(invitation_key);
  BEGIN
    INSERT INTO t_invitation (
      c_invitation_id,
      c_program_user_id,
      c_issuer_id,
      c_program_id,
      c_recipient_email,
      c_key_hash,
      c_email_id
    )
    VALUES (
      invitation_key_id,
      p_program_user_id,
      p_issuer_id,
      p_program_id,
      p_recipient_email,
      invitation_key_hash,
      p_email_id
    );
    return invitation_key_id || '.' || invitation_key;
  END;
$$ LANGUAGE plpgsql;