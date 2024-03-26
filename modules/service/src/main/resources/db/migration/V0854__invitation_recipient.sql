
-- Add recipient email to user invitations.

CREATE EXTENSION citext;
CREATE DOMAIN d_email AS citext
CHECK(
  --  N.B. this is the same pattern used in the EmailAddress class; if we change one we need to change the other.
  VALUE ~ '^\w+@[a-zA-Z_]+?\.[a-zA-Z]{2,3}$'
);

-- wipe out any existing invitations (we don't care)
DELETE FROM t_invitation;

ALTER TABLE t_invitation ADD
  c_recipient_email  d_email  not null;

CREATE OR REPLACE FUNCTION insert_invitation(
  p_issuer_id       d_user_id,                   
  p_program_id      d_program_id,         
  p_recipient_email d_email,       
  p_role            e_program_user_role,         
  p_support_type    e_program_user_support_type, 
  p_support_partner d_tag                  
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
      c_key_hash
    )
    VALUES (
      invitation_key_id,
      p_issuer_id,      
      p_program_id,     
      p_recipient_email,
      p_role,           
      p_support_type,   
      p_support_partner,
      invitation_key_hash
    );
    return invitation_key_id || '.' || invitation_key;
  END;
$$ LANGUAGE plpgsql;

