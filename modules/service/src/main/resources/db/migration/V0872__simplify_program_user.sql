
-- Observer becomes Coi (read-only)
ALTER TYPE e_program_user_role RENAME VALUE 'observer' TO 'coi_ro';

-- Remove support type and support partner from t_program_user
ALTER TABLE t_program_user DROP CONSTRAINT t_program_user_check;
ALTER TABLE t_program_user DROP CONSTRAINT t_program_user_c_support_partner_fkey;
ALTER TABLE t_program_user DROP column c_support_partner;
ALTER TABLE t_program_user DROP column c_support_type;;

-- Same, but with t_invitation
ALTER TABLE t_invitation DROP CONSTRAINT t_invitation_check;
ALTER TABLE t_invitation DROP CONSTRAINT t_invitation_c_support_partner_fkey;
ALTER TABLE t_invitation DROP column c_support_partner;
ALTER TABLE t_invitation DROP column c_support_type;;

-- There were actually a bunch of overloads, oops
DROP FUNCTION insert_invitation(d_user_id,d_program_id,e_program_user_role,e_program_user_support_type,d_tag);
DROP FUNCTION insert_invitation(d_user_id,d_program_id,d_email,e_program_user_role,e_program_user_support_type,d_tag);
DROP FUNCTION insert_invitation(d_user_id,d_program_id,d_email,e_program_user_role,e_program_user_support_type,d_tag,text);

CREATE FUNCTION insert_invitation(
  p_issuer_id       d_user_id,                   
  p_program_id      d_program_id,         
  p_recipient_email d_email,       
  p_role            e_program_user_role,         
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
      c_key_hash,
      c_email_id
    )
    VALUES (
      invitation_key_id,
      p_issuer_id,      
      p_program_id,     
      p_recipient_email,
      p_role,           
      invitation_key_hash,
      p_email_id
    );
    return invitation_key_id || '.' || invitation_key;
  END;
$$ LANGUAGE plpgsql;

-- No longer need e_program_user_support_type
DROP TYPE e_program_user_support_type;
