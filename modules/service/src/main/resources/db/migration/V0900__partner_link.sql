CREATE TYPE e_partner_link AS ENUM (
  'has_partner',
  'has_non_partner',
  'has_unspecified_partner'
);

ALTER TABLE t_program_user
  ADD COLUMN c_partner_link e_partner_link;

UPDATE t_program_user
  SET c_partner_link =
    CASE
      WHEN c_partner IS NOT NULL THEN 'has_partner'::e_partner_link
      ELSE 'has_unspecified_partner'::e_partner_link
    END;

ALTER TABLE t_program_user
  ALTER COLUMN c_partner_link SET NOT NULL,
  ADD CONSTRAINT program_user_partner_check CHECK (
    (c_partner IS NOT NULL AND c_partner_link = 'has_partner') OR
    (c_partner IS NULL AND c_partner_link != 'has_partner')
  );

ALTER TABLE t_invitation
  ADD COLUMN c_partner_link e_partner_link;

UPDATE t_invitation
  SET c_partner_link =
    CASE
      WHEN c_partner IS NOT NULL THEN 'has_partner'::e_partner_link
      ELSE 'has_unspecified_partner'::e_partner_link
    END;

ALTER TABLE t_invitation
  ALTER COLUMN c_partner_link SET NOT NULL,
  ADD CONSTRAINT invitation_partner_check CHECK (
    (c_partner IS NOT NULL AND c_partner_link = 'has_partner') OR
    (c_partner IS NULL AND c_partner_link != 'has_partner')
  );

DROP FUNCTION insert_invitation;

CREATE FUNCTION insert_invitation(
  p_issuer_id       d_user_id,
  p_program_id      d_program_id,
  p_recipient_email d_email,
  p_role            e_program_user_role,
  p_partner_link    e_partner_link,
  p_partner         d_tag,
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
      c_partner_link,
      c_partner,
      c_key_hash,
      c_email_id
    )
    VALUES (
      invitation_key_id,
      p_issuer_id,
      p_program_id,
      p_recipient_email,
      p_role,
      p_partner_link,
      p_partner,
      invitation_key_hash,
      p_email_id
    );
    return invitation_key_id || '.' || invitation_key;
  END;
$$ LANGUAGE plpgsql;